;;; db-markdown.el --- Markdown configurations

;;; Commentary:
;;

;;; Code:


;; ====================================
;; Markdown Configuration
;; ====================================

;; Set pandoc as the markdown processor
(custom-set-variables
 '(markdown-command "/usr/local/bin/pandoc"))

;; Function to convert markdown to HTML with strapdown.js
(defun markdown-html (buffer)
  "Convert markdown BUFFER content to HTML using strapdown.js for styling."
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" 
            (buffer-substring-no-properties (point-min) (point-max))))
	 (current-buffer)))

;; Function to create a styled HTML preview of markdown content
(defun my-markdown-filter (buffer)
  "Apply GitHub-style CSS to markdown preview of BUFFER content."
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

;; Function to preview markdown in real-time
(defun my-markdown-preview ()
  "Start a live preview server for the current markdown buffer."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

(provide 'db-markdown)
;;; db-markdown.el ends here
