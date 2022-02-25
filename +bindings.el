;;; +bindings.el -*- lexical-binding: t; -*-

(map!
 "s-k" #'kill-this-buffer
 "s-a" #'mark-whole-buffer
 "s-v" #'yank
 "s-c" #'kill-ring-save
 "s-s" #'save-buffer
 "s-l" #'goto-line
 "s-w" #'delete-frame
 "s-h" #'iconify-frame
 "s-z" #'undo
 "s-k" #'kill-this-buffer
 "s-s" #'save-buffer
 "s-n" #'make-frame
 "s-q" #'save-buffers-kill-emacs

 "C-0" #'toggle-frame-fullscreen

 "C-x w" #'winner-undo
 (:leader
  ;;:desc "switch buffer" :n "j" #'+ivy/switch-buffer
  ;;:desc "switch workspace buffer" :n "k" #'+ivy/switch-workspace-buffer
  :desc "switch" :n "SPC" #'(lambda (&optional invalidate-cache)
  "Jump to a project's file using completion.
With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((project-root (loop for i in projectile-known-projects
                             if (equal (+workspace-current-name) (doom-project-name i))
                             return i))
         (file (projectile-completing-read "Find file: "
                                           (projectile-project-files (s-replace "~" "/Users/royokong" project-root))))
         (ff #'find-file))
    (when file
      (funcall ff (expand-file-name file project-root))
      (run-hooks 'projectile-find-file-hook))))

  :desc "bibtex" :n "n b" #'helm-bibtex
  :desc "switch buffer" :n "j" #'switch-to-buffer
  :desc "switch workspace buffer" :n "k" #'+vertico/switch-workspace-buffer
  :desc "Org roam find file" :n "v" #'org-roam-node-find
  :desc "toggle last popup" :n "d" #'(lambda()
                                       (interactive)
                                       (let ((closep (+popup-windows)))
                                           (+popup/toggle)
                                           (if closep
                                               (balance-windows)))
                                       ))

 (:after company
  "M-/" #'company-complete)

 (:after org-capture
  "C-c c" #'org-capture)

 (:after org
  :map org-mode-map
  "C-'" nil)

 (:after ivy
  :map ivy-minibuffer-map
  "C-f" #'ivy-scroll-up-command
  "C-b" #'ivy-scroll-down-command)

 (:after avy
  "C-j" #'avy-goto-char;;-timer
  :n "s" #'avy-goto-char)

 "s-i" #'(lambda (args)
           (interactive "P")
           (if (eq major-mode 'vterm-mode)
                   (+vterm/toggle args)
                (+vterm/toggle args)
                (unless (eq major-mode 'vterm-mode) (+vterm/toggle args)))
           (when (s-contains?  "vterm" (buffer-name))
             (evil-insert 0))
           )
  ;;"s-i" #'(lambda ()
  ;;        (interactive)
  ;;        (if (eq major-mode 'vterm-mode)
  ;;        (+workspace/close-window-or-workspace)
  ;;        (if (get-buffer "vterm")
  ;;                (pop-to-buffer-same-window "vterm")
  ;;        (vterm))
  ;;        (evil-insert 0)))

 (:after telega
  :map telega-chat-mode-map
  "C-c C-r" #'telega-sticker-choose-favorite-or-recent)

 (:after ein
  :map ein:notebook-mode-map
  "C-c b" #'ein:worksheet-insert-cell-below
  :n "RET" #'ein:worksheet-execute-cell)

 (:after imenu-list
  "C-c u" #'imenu-list-smart-toggle
  :map imenu-list-major-mode-map
  "j" #'next-line
  "k" #'previous-line)

 (:after dired
  :map dired-mode-map
  :n "h" #'dired-up-directory
  :n "l" #'dired-find-file
  "s-o" #'(lambda ()
          (interactive)
          (dired-do-shell-command
           "open" nil
           (dired-get-marked-files t current-prefix-arg))))

 (:after evil
  "C-a" nil
  "C-a h" #'(lambda ()
              (interactive)
              (condition-case nil
                  (evil-window-left 1)
                (error (shell-command "tmux select-pane -L"))))

  "C-a j" #'(lambda ()
              (interactive)
              (condition-case nil
                  (evil-window-down 1)
                (error (shell-command "tmux select-pane -D"))))

  "C-a l" #'(lambda ()
              (interactive)
              (condition-case nil
                  (evil-window-right 1)
                (error (shell-command "tmux select-pane -R"))))

  "C-a k" #'(lambda ()
              (interactive)
              (condition-case nil
                  (evil-window-up 1)
                (error (shell-command "tmux select-pane -u")))))
 )
