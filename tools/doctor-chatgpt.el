(defvar doctor-chatgpt-process nil)
(defvar doctor-chatgpt-replying nil)
(defvar doctor-chatgpt-ready nil)
(defvar doctor-chatgpt-recv-list nil)
(defvar doctor-chatgpt-send-list nil)
(defvar doctor-chatgpt-send-start-point 0)
(defvar doctor-chatgpt-overlay nil)

(defcustom doctor-chatgpt-version 3
  "The version of chatgpt."
  :type 'int
  :group 'doctor-chatgpt)

(defcustom doctor-chatgpt-offical-key ""
  "The offical key for chatgpt."
  :type 'string
  :group 'doctor-chatgpt)

(defun doctor-chatgpt-save-chat-and-kill ()
  (interactive)
  (when-let ((recentf-keep '(".*" . nil)) ;; not push temp file in recentf-list
             (recentf-exclude '(".*"))
             (need-save (> (length doctor-chatgpt-recv-list) 1)))
    (write-file (expand-file-name
                 (concat "~/.doctor-chats/"
                         (format-time-string "%Y-%m-%d-%H:%M")))))
  (when (process-live-p doctor-chatgpt-process)
    (kill-process doctor-chatgpt-process))
  (kill-this-buffer))

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
                       "--api_key" doctor-chatgpt-offical-key "--version" (number-to-string doctor-chatgpt-version)))
  (setq doctor-chatgpt-ready nil)
  (setq doctor-chatgpt-replying t)
  (set-process-filter doctor-chatgpt-process 'doctor-chatgpt-filter))

(defun doctor-chatgpt-want-to-input (input)
  "Send INPUT to chatgpt."
  (cond
   ((and (process-live-p doctor-chatgpt-process)
         doctor-chatgpt-ready
         (not doctor-chatgpt-replying))
    (with-current-buffer "*doctor*"
      (goto-char (point-max))
      (insert input)
      (insert "\n")
      (doctor-chatgpt-read-print t)))
   (t
    (run-with-timer 0.1 nil #'doctor-chatgpt-want-to-input input))))

(defun doctor-chatgpt (&optional arg)
  "Switch to *doctor* buffer and start giving psychotherapy."
  (interactive (list (if (use-region-p)
                           (buffer-substring (region-beginning) (region-end))
                       nil)))
  (cond
   (arg
    (doctor-chatgpt)
    (doctor-chatgpt-want-to-input (format "%sExplain this code" arg)))
   (t
    (if (eq major-mode 'doctor-mode)
        (switch-to-prev-buffer)
      (switch-to-buffer "*doctor*")
      (when (eq (point-max) 1)
        ;; init
        (doctor-chatgpt-start-process)
        (doctor-mode))))))

(defun doctor-chatgpt-read-print (&optional not-use-face)
  "Top level loop."
  (interactive nil doctor-mode)
  (unless not-use-face
        (overlay-put (make-overlay doctor-chatgpt-send-start-point (point-max))
                     'face 'highlight))
  (setq doctor-sent
        (string-trim
         (buffer-substring-no-properties doctor-chatgpt-send-start-point (point-max))))
  (insert "\n")
  (push doctor-sent doctor-chatgpt-send-list)
  (setq doctor-chatgpt-replying t)
  (process-send-string doctor-chatgpt-process (concat doctor-sent "\n\n\n\n\n")))

(advice-add 'doctor :override #'doctor-chatgpt)
(advice-add 'doctor-read-print :override #'doctor-chatgpt-read-print)
(define-key doctor-mode-map (kbd "s-k") #'doctor-chatgpt-save-chat-and-kill)
(define-key doctor-mode-map (kbd "DEL") #'doctor-chatgpt-backward-delete-char)
(defun doctor-chatgpt-backward-delete-char ()
  (interactive)
  (when (> (point) doctor-chatgpt-send-start-point)
    (backward-delete-char 1)))

(after! evil-collection
  (evil-collection-define-key 'normal 'doctor-mode-map "q" #'doctor-chatgpt-save-chat-and-kill))

(provide 'doctor-chatgpt)
