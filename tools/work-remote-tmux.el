;;; work-remote-tmux.el -*- lexical-binding: t; -*-
(defun work-remote-tmux-start-sesstion (ip dir)
  (let ((vterm-session-name (format "%s-%s"
                                    (nth 0 (split-string ip "\\."))
                                    dir))
        (vterm-in-window nil))
    (cond
     ((dolist (window (window-list) vterm-in-window)
        (when (equal
               vterm-session-name
               (with-current-buffer (window-buffer window)
                 (buffer-name)))
          (message (buffer-name))
          (setq vterm-in-window window)))
      (select-window vterm-in-window))

     ((get-buffer vterm-session-name)
      (switch-to-buffer vterm-session-name))

     (t
      (vterm vterm-session-name)
      (with-current-buffer vterm-session-name
        (setq-local truncate-lines nil))
      (vterm-send-string
       (format "bash /Users/royokong/work_remote_tmux.sh %s %s;exit" ip dir))
      (vterm-send-return)))))


(defun work-remote-tmux-start-open (ip)
  (let* ((full-dir (cl-loop for i in projectile-known-projects
                            if (equal (+workspace-current-name) (doom-project-name i))
                            return i))
         (dir (nth 0 (last (split-string
                           (string-trim full-dir "\\/" "\\/")
                           "\\/")))))
    (work-remote-tmux-start-sesstion ip dir)))

(defun work-remote-tmux-start-open-172 ()
  (interactive)
  (work-remote-tmux-start-open work-remote-server-172))

(defun work-remote-tmux-start-open-192 ()
  (interactive)
  (work-remote-tmux-start-open work-remote-server-192))

(defvar work-remote-tumx-last-buffer nil)

(defun work-remote-tmux-toggle (ip)
  (cond ((and (eq major-mode 'vterm-mode)
              (string-match-p
               (format "%s-" (nth 0 (split-string ip "\\.")))
               (buffer-name)))
         (if work-remote-tumx-last-buffer
             (switch-to-buffer work-remote-tumx-last-buffer)
           (message "No last buffer")))
        (t
         (let ((buffer-name  (current-buffer))
               (buffer-mode major-mode))
           (work-remote-tmux-start-open ip)
           (unless (equal buffer-mode 'vterm-mode)
             (setq-local work-remote-tumx-last-buffer buffer-name))
           (evil-insert 0)))))

(defvar work-remote-tmux-rerun--start-point nil)
(defvar work-remote-tmux-rerun--prompt-command nil)
(defvar work-remote-tmux-rerun--prompt nil)
(defvar work-remote-tmux-rerun--buffer nil)
(defvar work-remote-tmux-rerun--wait-timer-count nil)
(defvar work-remote-tmux-rerun--wait-timer-count nil)


(defun vterm-remote-tmux-rerun--check-end ()
  (let ((current-line
         (if (buffer-live-p work-remote-tmux-rerun--buffer)
             (with-current-buffer work-remote-tmux-rerun--buffer
               (buffer-substring-no-properties (vterm--get-beginning-of-line) (vterm--get-end-of-line)))
           nil))
        (cbuffer (compilation-buffer-name "compilation" nil nil)))
    (cond ((eq (string-match work-remote-tmux-rerun--prompt current-line) 0)
           ;; end
           (with-current-buffer (get-buffer-create cbuffer)
             (read-only-mode -1)
             (erase-buffer)
             (insert
              (string-replace "/home/jt" "/Users/royokong/nlp"
                              (with-current-buffer work-remote-tmux-rerun--buffer
                                (buffer-substring-no-properties (point-min) (point-max)))))

             (goto-char (point-min))
             (when (search-forward work-remote-tmux-rerun--prompt-command nil t)
               (delete-region (point-min) (point)))
             (goto-char (point-max))
             (while (search-backward-regexp work-remote-tmux-rerun--prompt nil t))
             (delete-region (point) (point-max))
             (read-only-mode 1)
             (compilation-mode))
           (+popup-buffer (get-buffer cbuffer)))
          ;; wait
          ((and (> work-remote-tmux-rerun--wait-timer-count 0) current-line)
           (message "%s(%ss): %s"
                    (buffer-name work-remote-tmux-rerun--buffer)
                    (* (- 600 work-remote-tmux-rerun--wait-timer-count) 0.5)
                    (if (eq 0 (length current-line))
                        (with-current-buffer work-remote-tmux-rerun--buffer
                          (save-excursion
                            (when (eq 0 (length (buffer-substring-no-properties (vterm--get-beginning-of-line) (vterm--get-end-of-line))))
                                (previous-line))
                            (buffer-substring-no-properties (vterm--get-beginning-of-line) (vterm--get-end-of-line))))
                      current-line))
           (cl-decf work-remote-tmux-rerun--wait-timer-count)
           (run-with-timer 0.5 nil #'vterm-remote-tmux-rerun--check-end)))
      ))


(defun work-remote-tmux-rerun  (args)
  (interactive "P")
  (save-buffer)
  (if startsync-running (run-with-timer 0.1 nil 'work-remote-tmux-rerun args)
    (let ((l-vterm-window nil)
          (window-conf (current-window-configuration)))

      (dolist (window (window-list))
        (when  (or (cl-search "172-" (buffer-name (window-buffer window)))
                   (cl-search "192-" (buffer-name (window-buffer window))))
          (setq l-vterm-window window)))

      (cond
       (l-vterm-window
        (select-window  l-vterm-window))
       ((or (not work-remote-tmux-rerun--buffer)
            (cl-search "172-" (buffer-name work-remote-tmux-rerun--buffer)))
        (work-remote-tmux-toggle work-remote-server-172))
       ((or (not work-remote-tmux-rerun--buffer)
            (cl-search "192-" (buffer-name work-remote-tmux-rerun--buffer)))
        (work-remote-tmux-toggle work-remote-server-192)))

      ;; kill current buffer
      (vterm-send-key "c" nil nil t)
      (sleep-for 0.05)
      (vterm-send-key "d" nil nil t)
      (sleep-for 0.05)
      (vterm-send-return)
      (sleep-for 0.1)

      (setq work-remote-tmux-rerun--start-point (point))
      (setq work-remote-tmux-rerun--buffer (current-buffer))
      (setq work-remote-tmux-rerun--wait-timer-count 600)

      (setq work-remote-tmux-rerun--prompt "^.* ➜ .* ✗ $")

      (vterm-send-key "p" nil nil t)
      (sleep-for 0.1)

      (setq work-remote-tmux-rerun--prompt-command
            (string-trim-right
             (buffer-substring-no-properties (vterm--get-beginning-of-line) (vterm--get-end-of-line))))

      (vterm-send-return)

      (run-with-timer 0.5 nil #'vterm-remote-tmux-rerun--check-end)
      (set-window-configuration window-conf))))


(defun work-remote-tmux-get-jobs ()
  (interactive)
  (let ((data (json-read-file "/Users/royokong/running_job.json"))
        (jobs '()))
    (dolist (job data)
      (let* ((jd (cdr job))
            (cmd (cdr (assoc 'cmd jd)))
            (pid (cdr (assoc 'pid jd)))
            (start_time (cdr (assoc 'start_time jd)))
            (running (cdr (assoc 'running jd)))
            (server (cdr (assoc 'server jd))))
        (if (eq running t)
            (push (list cmd pid start_time server pid) jobs))))
    jobs))



(defun work-remote-tmux-get-jobs ()
  (let* ((data (json-read-file "/Users/royokong/running_job.json"))
         (completions (make-hash-table :test 'equal :size (length data))))
    ;; convert vector to list
    (dolist (jd (append data nil))
      (let* ((cmd (cdr (assoc 'cmd jd)))
             (pid (cdr (assoc 'pid jd)))
             (run_name (cdr (assoc 'run_name jd)))
             (gpuid (cdr (assoc 'gpuid jd)))
             (start_time (cdr (assoc 'start_time jd)))
             (server (cdr (assoc 'server jd))))
        (puthash
         (concat
          (format "%s\t%s\t%s-%s\t%s\t"
                  pid start_time
                  (substring server 3 6) gpuid
                  run_name)
          cmd )
         jd completions)
      ))
    completions))


(defvar work-remote-tmux-update-job-timer nil)
(defvar work-remote-tmux-current-pids nil)
(defvar work-remote-tmux-current-jobs nil)

(defun work-remote-tmux-update-job ()
  (unless (get-buffer "*update_jobs*")
    (start-process "*update_jobs*" "*update_jobs*" "python" "/Users/royokong/.doom.d/tools/get-running-job.py")
    (set-process-sentinel
     (get-buffer-process "*update_jobs*")
     (lambda (p e)
       ;;(message "update job done")
       (when (= 0 (process-exit-status p))
         (let ((buf (process-buffer p)))
           (kill-buffer buf))

         (let* ((data (append (json-read-file "/Users/royokong/running_job.json") nil))
                (pids
                 (cl-loop for job in data
                          collect  (cdr (assoc 'pid job))))
                (jobs (make-hash-table :test 'equal :size (length pids) )))

           (dolist (job data)
             (puthash (cdr (assoc 'pid job)) job jobs))

           (dolist (pid pids)
             (unless (member pid work-remote-tmux-current-pids)
               (message "start job %s %s-%s %s" pid
                        (substring (cdr (assoc 'server (gethash pid jobs))) 3 6)
                        (cdr (assoc 'gpuid (gethash pid jobs)))
                        (if (> (length (cdr (assoc 'run_name (gethash pid jobs)))) 0)
                            (cdr (assoc 'run_name (gethash pid jobs)))
                          (cdr (assoc 'cmd (gethash pid jobs)))))))

           (if work-remote-tmux-current-pids
               (dolist (pid work-remote-tmux-current-pids)
                 (unless (member pid pids)
                   (message "finish job %s %s-%s %s" pid
                            (substring (cdr (assoc 'server (gethash pid work-remote-tmux-current-jobs))) 3 6)
                            (cdr (assoc 'gpuid (gethash pid work-remote-tmux-current-jobs)))
                            (if (> (length (cdr (assoc 'run_name (gethash pid work-remote-tmux-current-jobs)))) 0)
                                (cdr (assoc 'run_name (gethash pid work-remote-tmux-current-jobs)))
                              (cdr (assoc 'cmd (gethash pid work-remote-tmux-current-jobs))))))))

           (setq work-remote-tmux-current-pids pids
                 work-remote-tmux-current-jobs jobs))
         )))))


(defun work-remote-tmux-update-job-start ()
  (interactive)
  (when work-remote-tmux-update-job-timer
    (cancel-timer work-remote-tmux-update-job-timer))
  (setq work-remote-tmux-update-job-timer
        (run-with-timer 0.1 30 #'work-remote-tmux-update-job)))

(defun work-remote-tmux--restart-job (pid gpuid cmd)
  (if (< (vterm--get-cursor-point) 500)
      (run-with-timer 0.5 nil #'work-remote-tmux--restart-job pid gpuid cmd)
    (vterm-send-key "b" nil nil t)
    (sleep-for 0.1)
    (vterm-send-key "c")
    (sleep-for 0.1)
    (vterm-send-string "conda activate")
    (vterm-send-return)
    (sleep-for 0.1)
    (vterm-send-string (format "kill -9 %s" pid))
    (vterm-send-return)
    (sleep-for 0.1)
    (vterm-send-string (format "CUDA_VISIBLE_DEVICES=%s %s" gpuid cmd))
    (vterm-send-return))

  (run-with-timer 0.1 nil #'work-remote-tmux-update-job)
)

(defun work-remote-tmux-restart-job (job)
  (interactive (list (completing-read "Job: " (work-remote-tmux-get-jobs))))
  (when  (y-or-n-p (format "Restart job %s" job))
      (let* ((jd (gethash job (work-remote-tmux-get-jobs)))
             (pid (cdr (assoc 'pid jd)))
             (cmd (cdr (assoc 'cmd jd)))
             (gpuid (cdr (assoc 'gpuid jd)))
             (server (cdr (assoc 'server jd)))
             (ip (nth 1 (split-string server "@"))))
        (work-remote-tmux-start-open ip)
        (work-remote-tmux--restart-job pid gpuid cmd))))


(defun work-remote-tmux-restart-job-with-other-cmd (job)
  (interactive (list (completing-read "Job: " (work-remote-tmux-get-jobs))))
  (when (y-or-n-p (format "Restart job %s" job))
    (let* ((jd (gethash job (work-remote-tmux-get-jobs)))
           (pid (cdr (assoc 'pid jd)))
           (cmd (read-string "CMD: "))
           (gpuid (cdr (assoc 'gpuid jd)))
           (server (cdr (assoc 'server jd)))
           (ip (nth 1 (split-string server "@"))))
      (work-remote-tmux-start-open ip)
      (work-remote-tmux--restart-job pid gpuid cmd))))


(map!
  "s-i" #'(lambda (args)
            (interactive "P")
            (work-remote-tmux-toggle work-remote-server-172))

  "s-I" #'(lambda (args)
            (interactive "P")
            (other-window 1)
            (while (equal (buffer-name) "*sort-tab*")
              (other-window 1))
            (work-remote-tmux-toggle work-remote-server-172))


  "s-r" #'work-remote-tmux-rerun

  "s-R" #'work-remote-tmux-restart-job

  "s-j" #'(lambda (args)
            (interactive "P")
            (work-remote-tmux-toggle work-remote-server-192))

  "s-J" #'(lambda (args)
            (interactive "P")
            (other-window 1)
            (while (equal (buffer-name) "*sort-tab*")
              (other-window 1))
            (work-remote-tmux-toggle work-remote-server-192)))



(after! evil-collection
  (evil-collection-define-key 'normal  'vterm-mode-map
    (kbd "C-b") #'(lambda ()
                    (interactive)
                    (vterm--self-insert)
                    (evil-collection-vterm-insert)))
  (evil-collection-define-key 'insert 'vterm-mode-map
    (kbd "C-w") nil
    (kbd "C-w k") #'evil-window-up
    (kbd "C-w j") #'evil-window-down
    (kbd "C-w h") #'my-evil-move-left-window
    (kbd "C-w l") #'my-evil-move-right-window
    (kbd "C-w C-h") #'my-evil-move-left-window
    (kbd "C-w C-l") #'my-evil-move-right-window))

;; start timer to update jobs
(work-remote-tmux-update-job-start)

(defun work-remote-tmux-or-vterm-quit ()
  (interactive)
  (let ((bname (buffer-name)))
    (cond
     ((string-match-p "172-" bname) (work-remote-tmux-toggle work-remote-server-172))
     ((string-match-p "192-" bname) (work-remote-tmux-toggle work-remote-server-192))
     (t (vterm-pop-posframe-toggle)))))

(evil-collection-define-key 'normal  'vterm-mode-map "q" #'work-remote-tmux-or-vterm-quit)

(provide 'work-remote-tmux)
