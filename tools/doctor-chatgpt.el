(defvar doctor-chatgpt-process nil)
(defvar doctor-chatgpt-replying nil)
(defvar doctor-chatgpt-ready nil)
(defvar doctor-chatgpt-recv-list nil)
(defvar doctor-chatgpt-send-list nil)
(defvar doctor-chatgpt-send-start-point 0)
(defvar doctor-chatgpt-overlay nil)

(defcustom doctor-chatgpt-offical-key ""
  "The offical key for chatgpt."
  :type 'string
  :group 'doctor-chatgpt)

(defun doctor-chatgpt-filter (process output)
  "Filter for chatgpt process."
  (let ((buffer (process-buffer process)))
    (cond
     ((string-match "Logging in\.\.\." output)
      (setq doctor-chatgpt-ready t))
     ((not doctor-chatgpt-ready))
     ((equal output "Chatbot: \n")
      (setq doctor-chatgpt-replying t)
      (with-current-buffer "*doctor*" (read-only-mode 1)))
     (t
      (when-let* ((el (string-match "\n+You:\n+$" output)))
        (setq doctor-chatgpt-replying nil)
        (setq output (substring output 0 el)))
      (when (> (length output) 1) (push output doctor-chatgpt-recv-list))
      (with-current-buffer "*doctor*"
        (read-only-mode -1)
        (goto-char (point-max))
        (insert output)
        (if doctor-chatgpt-replying
            (read-only-mode 1)
          (if doctor-chatgpt-recv-list (insert "\n\n"))
          (with-current-buffer "*doctor*"
            (setq doctor-chatgpt-send-start-point (point-max)))))))))

(defun doctor-chatgpt-start-process ()
  "Start a chat with ChatGPT."
  (when (and (processp doctor-chatgpt-process) (process-live-p doctor-chatgpt-process))
    (kill-process doctor-chatgpt-process))
  (setq doctor-chatgpt-recv-list nil)
  (setq doctor-chatgpt-send-list nil)
  (setq doctor-chatgpt-send-start-point 114)  ;; skip doctor prompt
  (setq doctor-chatgpt-process
        (start-process "*doctor-chatgpt*" "*doctor-chatgpt*"
                       "/opt/homebrew/bin/python3" (expand-file-name "~/.doom.d/tools/revChatGPT_wrap.py")
                       "--api_key" doctor-chatgpt-offical-key))
  (setq doctor-chatgpt-ready nil)
  (set-process-filter doctor-chatgpt-process 'doctor-chatgpt-filter))

(defun doctor-chatgpt-read-print ()
  "Top level loop."
  (interactive nil doctor-mode)
  (overlay-put (make-overlay doctor-chatgpt-send-start-point (point-max))
               'face 'highlight)
  (setq doctor-sent
        (string-trim
         (buffer-substring-no-properties doctor-chatgpt-send-start-point (point-max))))
  (insert "\n")
  (push doctor-sent doctor-chatgpt-send-list)
  (setq doctor-chatgpt-replying t)
  (process-send-string doctor-chatgpt-process (concat doctor-sent "\n\n")))

(advice-add 'doctor :before #'doctor-chatgpt-start-process)
(advice-add 'doctor-read-print :override #'doctor-chatgpt-read-print)

(provide 'doctor-chatgpt)
