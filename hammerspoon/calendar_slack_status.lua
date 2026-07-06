-- calendar_slack_status.lua
-- Syncs Google Calendar -> Slack status, skipping "Focus Time" events.
--
-- Behavior:
--   * A non-focus event is currently active  -> set Slack status "In a meeting",
--     expiring at the event's end time.
--   * A "Focus Time" (or any "Focus") event is active, and no real meeting
--     is active -> do nothing (leave Slack status untouched).
--   * Nothing active -> do nothing (Slack auto-expires the previous status).
--
-- Handles RRULE (FREQ=DAILY / FREQ=WEEKLY, with INTERVAL / BYDAY / UNTIL / COUNT)
-- and UTC ("Z"), all-day (VALUE=DATE), and floating/local DTSTART/DTEND values.
--
-- Secrets (Slack token + secret ICS URL) are NEVER stored in this file. They are
-- read from 1Password via the `op` CLI, with a gitignored secrets.lua fallback.

------------------------------------------------------------------------
-- CONFIG (non-secret)
------------------------------------------------------------------------

-- 1Password secret references. Replace with your own: in 1Password, right-click
-- the field -> "Copy Secret Reference" to get an op://Vault/Item/field path.
local SLACK_TOKEN_REF = "op://Private/Slack Status Token/credential"
local ICS_URL_REF     = "op://Private/Calendar Secret URL/credential"

local STATUS_TEXT   = "In a meeting"
local STATUS_EMOJI  = ":calendar:"
local FOCUS_MATCH   = "focus"          -- lowercase substring that means "skip"
local POLL_SECONDS  = 5 * 60           -- check every ~5 minutes

------------------------------------------------------------------------
-- SECRET LOADING (1Password, with gitignored secrets.lua fallback)
------------------------------------------------------------------------

-- Read one secret from 1Password. Runs in the user's login shell (second arg
-- `true`) so `op` is on PATH and any unlocked-session env vars are present.
local function opRead(ref)
  if not ref or ref:sub(1, 5) ~= "op://" then return nil end
  local out, ok = hs.execute("op read '" .. ref .. "' 2>/dev/null", true)
  if ok and out then
    out = out:gsub("%s+$", "")
    if out ~= "" then return out end
  end
  return nil
end

-- Fallback: a gitignored file at ~/.hammerspoon/secrets.lua that returns
--   return { slack_token = "xoxp-...", ics_url = "https://.../basic.ics" }
local function fromSecretsFile()
  local path = hs.configdir .. "/secrets.lua"
  if not hs.fs.attributes(path) then return nil, nil end
  local ok, s = pcall(dofile, path)
  if ok and type(s) == "table" then return s.slack_token, s.ics_url end
  return nil, nil
end

local cachedToken, cachedICS
local function getSecrets()
  if cachedToken and cachedICS then return cachedToken, cachedICS end
  local token = opRead(SLACK_TOKEN_REF)
  local ics   = opRead(ICS_URL_REF)
  if not token or not ics then
    local fToken, fICS = fromSecretsFile()
    token = token or fToken
    ics   = ics or fICS
  end
  if token and ics then cachedToken, cachedICS = token, ics end
  return token, ics
end

------------------------------------------------------------------------
-- TIME HELPERS
------------------------------------------------------------------------

-- Convert a broken-down time table that represents UTC wall-clock into an
-- absolute epoch. os.time assumes *local* time, so we correct by the local
-- UTC offset at that instant. Critically, we clear isdst (os.date("!*t")
-- returns isdst=false) so os.time auto-determines daylight saving for that
-- date — otherwise UTC events land one hour off during DST.
local function utcTableToEpoch(tbl)
  local guess = os.time(tbl) -- tbl interpreted as local time
  local u = os.date("!*t", guess) -- same instant expressed in UTC
  u.isdst = nil -- let os.time auto-determine DST (avoids the PST/PDT off-by-one)
  local off = os.difftime(guess, os.time(u))
  return guess + off
end

-- Parse an ICS datetime value into an epoch. Values ending in Z are UTC;
-- everything else (TZID or floating) is treated as the machine's local time.
-- All-day dates (8 digits, no "T") -> local midnight.
local function parseICSTime(val)
  val = val:gsub("%s+$", "")
  local isUTC = val:sub(-1) == "Z"
  if isUTC then val = val:sub(1, -2) end

  local y  = tonumber(val:sub(1, 4))
  local mo = tonumber(val:sub(5, 6))
  local d  = tonumber(val:sub(7, 8))
  local h, mi, s = 0, 0, 0
  local tpart = val:match("T(%d+)")
  if tpart then
    h  = tonumber(tpart:sub(1, 2)) or 0
    mi = tonumber(tpart:sub(3, 4)) or 0
    s  = tonumber(tpart:sub(5, 6)) or 0
  end
  if not (y and mo and d) then return nil end

  local tbl = { year = y, month = mo, day = d, hour = h, min = mi, sec = s }
  if isUTC then
    return utcTableToEpoch(tbl)
  else
    return os.time(tbl)
  end
end

-- Epoch of local midnight for the day containing `t`.
local function dayStart(t)
  local d = os.date("*t", t)
  return os.time({ year = d.year, month = d.month, day = d.day, hour = 0, min = 0, sec = 0 })
end

-- Whole-day difference between two epochs (b's day minus a's day).
local function dayDiff(a, b)
  return math.floor((dayStart(b) - dayStart(a)) / 86400 + 0.5)
end

-- ICS BYDAY code -> os.date wday (Sunday = 1 .. Saturday = 7)
local WDAY = { SU = 1, MO = 2, TU = 3, WE = 4, TH = 5, FR = 6, SA = 7 }

------------------------------------------------------------------------
-- ICS PARSING
------------------------------------------------------------------------

-- Unfold RFC 5545 line folding: a line beginning with a space or tab is a
-- continuation of the previous line.
local function unfold(body)
  body = body:gsub("\r\n", "\n")
  body = body:gsub("\n[ \t]", "")
  return body
end

-- Parse "RRULE:FREQ=WEEKLY;BYDAY=MO,WE;INTERVAL=2;UNTIL=20260901T000000Z"
local function parseRRule(line)
  local rule = {}
  for k, v in line:gmatch("([A-Z]+)=([^;]+)") do
    rule[k] = v
  end
  rule.INTERVAL = tonumber(rule.INTERVAL) or 1
  rule.COUNT    = tonumber(rule.COUNT)
  if rule.UNTIL then rule.UNTIL = parseICSTime(rule.UNTIL) end
  if rule.BYDAY then
    local set = {}
    for code in rule.BYDAY:gmatch("[A-Z][A-Z]") do
      if WDAY[code] then set[WDAY[code]] = true end
    end
    rule.BYDAY = set
  end
  return rule
end

-- Split the ICS into individual VEVENT blocks and extract the fields we need.
local function parseEvents(body)
  body = unfold(body)
  local events = {}
  for block in body:gmatch("BEGIN:VEVENT(.-)END:VEVENT") do
    local ev = {}
    ev.summary = block:match("\nSUMMARY[^:]*:([^\n]*)") or
                 block:match("^SUMMARY[^:]*:([^\n]*)") or ""
    local dtstart = block:match("DTSTART[^:\n]*:([^\n]*)")
    local dtend   = block:match("DTEND[^:\n]*:([^\n]*)")
    local rrule   = block:match("RRULE:([^\n]*)")

    ev.rawStart, ev.rawEnd, ev.rawRRule = dtstart, dtend, rrule -- for debugging
    if dtstart then ev.startT = parseICSTime(dtstart) end
    if dtend then
      ev.endT = parseICSTime(dtend)
    elseif ev.startT then
      -- No DTEND: all-day date -> +24h; otherwise treat as zero-length.
      ev.endT = ev.startT + (dtstart:find("T") and 0 or 86400)
    end
    if rrule then ev.rrule = parseRRule(rrule) end

    if ev.startT and ev.endT then
      events[#events + 1] = ev
    end
  end
  return events
end

------------------------------------------------------------------------
-- OCCURRENCE CHECK
------------------------------------------------------------------------

-- Is event `ev` active at epoch `now`? Expands DAILY/WEEKLY recurrence for
-- today (and yesterday, to catch occurrences that cross midnight).
local function isActive(ev, now)
  local duration = ev.endT - ev.startT
  if duration < 0 then duration = 0 end

  if not ev.rrule then
    return now >= ev.startT and now <= ev.endT
  end

  local rule = ev.rrule
  local freq = rule.FREQ
  if freq ~= "DAILY" and freq ~= "WEEKLY" then
    -- Unsupported frequency: fall back to the base occurrence only.
    return now >= ev.startT and now <= ev.endT
  end

  local base = os.date("*t", ev.startT) -- original wall-clock time-of-day (local)

  for _, offset in ipairs({ 0, -1 }) do
    local day = os.date("*t", now + offset * 86400)
    local candStart = os.time({
      year = day.year, month = day.month, day = day.day,
      hour = base.hour, min = base.min, sec = base.sec,
    })

    -- Occurrence can't precede the series start.
    if candStart >= dayStart(ev.startT) then
      local diff = dayDiff(ev.startT, candStart)
      local ok = false

      if freq == "DAILY" then
        ok = diff >= 0 and (diff % rule.INTERVAL == 0)
      elseif freq == "WEEKLY" then
        local wday = tonumber(os.date("%w", candStart)) + 1 -- %w: Sun=0 -> 1
        local weekOK = (math.floor(diff / 7) % rule.INTERVAL == 0)
        local dayOK
        if rule.BYDAY then
          dayOK = rule.BYDAY[wday] == true
        else
          dayOK = (wday == (tonumber(os.date("%w", ev.startT)) + 1))
        end
        ok = weekOK and dayOK and diff >= 0
      end

      if ok and rule.UNTIL and candStart > rule.UNTIL then ok = false end
      if ok and rule.COUNT then
        local index = (freq == "DAILY") and math.floor(diff / rule.INTERVAL)
                                          or math.floor(diff / 7 / rule.INTERVAL)
        if index >= rule.COUNT then ok = false end
      end

      if ok and now >= candStart and now <= candStart + duration then
        return true
      end
    end
  end

  return false
end

------------------------------------------------------------------------
-- SLACK
------------------------------------------------------------------------

local lastSignature = nil -- avoid re-POSTing the same status every cycle

local function setSlackStatus(token, text, emoji, expiration)
  local sig = text .. "|" .. emoji .. "|" .. tostring(expiration)
  if sig == lastSignature then return end

  local payload = hs.json.encode({
    profile = {
      status_text = text,
      status_emoji = emoji,
      status_expiration = expiration or 0,
    },
  })
  local headers = {
    ["Authorization"] = "Bearer " .. token,
    ["Content-Type"]  = "application/json; charset=utf-8",
  }
  hs.http.asyncPost("https://slack.com/api/users.profile.set", payload, headers,
    function(status, respBody)
      if status == 200 then
        local resp = hs.json.decode(respBody or "{}") or {}
        if resp.ok then
          lastSignature = sig
          print("[cal->slack] status set: " .. text)
        else
          print("[cal->slack] Slack API error: " .. tostring(resp.error))
        end
      else
        print("[cal->slack] HTTP " .. tostring(status) .. " from Slack")
      end
    end)
end

------------------------------------------------------------------------
-- MAIN LOOP
------------------------------------------------------------------------

local function poll()
  local token, icsUrl = getSecrets()
  if not token or not icsUrl then
    print("[cal->slack] secrets unavailable (1Password locked? or secrets.lua missing) — will retry")
    return
  end

  hs.http.asyncGet(icsUrl, nil, function(status, body)
    if status ~= 200 or not body then
      print("[cal->slack] ICS fetch failed: HTTP " .. tostring(status))
      return
    end

    local now = os.time()
    local events = parseEvents(body)

    local activeMeeting, activeFocus = nil, false
    for _, ev in ipairs(events) do
      if isActive(ev, now) then
        if ev.summary:lower():find(FOCUS_MATCH, 1, true) then
          activeFocus = true
        else
          -- Prefer the meeting ending soonest (most specific current block).
          if not activeMeeting or ev.endT < activeMeeting.endT then
            activeMeeting = ev
          end
        end
      end
    end

    if activeMeeting then
      setSlackStatus(token, STATUS_TEXT, STATUS_EMOJI, activeMeeting.endT)
    elseif activeFocus then
      print("[cal->slack] Focus Time active — leaving Slack status untouched")
    else
      -- Nothing active: do nothing. Slack expires the old status on its own.
      lastSignature = nil
    end
  end)
end

------------------------------------------------------------------------
-- DEBUG HELPER
------------------------------------------------------------------------

-- Run `calendarSlackDebug()` in the Hammerspoon console to see what the feed
-- parses to and which events are considered active right now.
function calendarSlackDebug()
  local _, icsUrl = getSecrets()
  if not icsUrl then print("[debug] no ICS URL (secrets missing)"); return end
  hs.http.asyncGet(icsUrl, nil, function(status, body)
    print("[debug] ICS HTTP " .. tostring(status) .. ", bytes " .. (body and #body or 0))
    if status ~= 200 or not body then return end
    local now = os.time()
    local events = parseEvents(body)
    print(string.format("[debug] parsed %d VEVENTs; now = %s",
      #events, os.date("%Y-%m-%d %H:%M:%S %Z", now)))
    -- Dump raw + parsed data for any event that could plausibly touch today,
    -- so we can see why an active meeting isn't matching.
    local todayW = tonumber(os.date("%w", now)) + 1
    local shown = 0
    for _, ev in ipairs(events) do
      local relevant = false
      if ev.rrule then
        local f = ev.rrule.FREQ
        if f == "DAILY" then relevant = true
        elseif f == "WEEKLY" then
          if ev.rrule.BYDAY then relevant = ev.rrule.BYDAY[todayW] == true
          else relevant = (tonumber(os.date("%w", ev.startT)) + 1) == todayW end
        end
      elseif ev.startT then
        relevant = math.abs(ev.startT - now) < 86400
      end
      if relevant and shown < 30 then
        shown = shown + 1
        print(string.format("[debug] %s active=%s | %q\n         rawStart=%s rawEnd=%s rawRRule=%s\n         startT=%s endT=%s",
          ev.rrule and "RECUR" or "ONE  ", tostring(isActive(ev, now)), ev.summary,
          tostring(ev.rawStart), tostring(ev.rawEnd), tostring(ev.rawRRule),
          ev.startT and os.date("%Y-%m-%d %H:%M:%S", ev.startT) or "nil",
          ev.endT and os.date("%Y-%m-%d %H:%M:%S", ev.endT) or "nil"))
      end
    end
    if shown == 0 then print("[debug] no events relevant to today") end
  end)
end

-- Run `calendarSlackFind("part of the title")` to dump the raw feed entries
-- for a specific event, revealing exactly how its time/recurrence is encoded.
function calendarSlackFind(needle)
  local _, icsUrl = getSecrets()
  if not icsUrl then print("[find] no ICS URL"); return end
  needle = needle:lower()
  hs.http.asyncGet(icsUrl, nil, function(status, body)
    if status ~= 200 or not body then print("[find] HTTP " .. tostring(status)); return end
    body = body:gsub("\r\n", "\n"):gsub("\n[ \t]", "")
    local n = 0
    for block in body:gmatch("BEGIN:VEVENT(.-)END:VEVENT") do
      local sum = block:match("SUMMARY[^:\n]*:([^\n]*)") or ""
      if sum:lower():find(needle, 1, true) then
        n = n + 1
        print("[find] --- match " .. n .. " ---")
        for _, key in ipairs({ "SUMMARY", "DTSTART", "DTEND", "RRULE",
                               "RECURRENCE-ID", "EXDATE", "STATUS" }) do
          for line in block:gmatch("[^\n]+") do
            if line:find("^" .. key) then print("   " .. line) end
          end
        end
      end
    end
    print("[find] " .. n .. " matching VEVENT(s) for '" .. needle .. "'")
  end)
end

------------------------------------------------------------------------
-- START
------------------------------------------------------------------------

calendarSlackTimer = hs.timer.doEvery(POLL_SECONDS, poll)
poll() -- run immediately on load
print("[cal->slack] calendar -> Slack status sync started")
