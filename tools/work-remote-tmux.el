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
  (work-remote-tmux-start-open "172.17.62.88"))

(defun work-remote-tmux-start-open-192 ()
  (interactive)
  (work-remote-tmux-start-open "192.168.1.115"))

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

(defun work-remote-tmux-rerun  (args)
  (interactive "P")
  (save-buffer)
  (if startsync-running (run-with-timer 0.1 nil 'work-remote-tmux-rerun args)
    (let ((swindow (selected-window))
          (l-vterm-window nil))
      (dolist (window (window-list))
        (when  (or (cl-search "172-" (buffer-name (window-buffer window)))
                   (cl-search "192-" (buffer-name (window-buffer window))))
          (setq l-vterm-window window)))
      (when l-vterm-window
        (other-window 1)
        (while (equal (buffer-name) "*sort-tab*")
          (other-window 1)))
      (unless l-vterm-window
        (work-remote-tmux-toggle "172.17.62.88"))
      ;;(if (cl-search "172-" (buffer-name))
      ;;    (work-remote-tmux-toggle "172.17.62.88")
      ;;  (work-remote-tmux-toggle "192.168.1.115"))
      (vterm-send-key "c" nil nil t)
      (sleep-for 0.1)
      (vterm-send-key "d" nil nil t)
      (sleep-for 0.1)
      (vterm-send-key "p" nil nil t)
      (vterm-send-return)
      (select-window swindow))))


(map!
  "s-i" #'(lambda (args)
            (interactive "P")
            (work-remote-tmux-toggle "172.17.62.88"))

  "s-I" #'(lambda (args)
            (interactive "P")
            (other-window 1)
            (while (equal (buffer-name) "*sort-tab*")
              (other-window 1))
            (work-remote-tmux-toggle "172.17.62.88"))


  "s-r" #'work-remote-tmux-rerun

  "s-j" #'(lambda (args)
            (interactive "P")
            (work-remote-tmux-toggle "192.168.1.115"))

  "s-J" #'(lambda (args)
            (interactive "P")
            (other-window 1)
            (while (equal (buffer-name) "*sort-tab*")
              (other-window 1))
            (work-remote-tmux-toggle "192.168.1.115")))



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

(provide 'work-remote-tmux)
