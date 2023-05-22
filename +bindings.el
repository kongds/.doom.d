;;; +bindings.el -*- lexical-binding: t; -*-
(defun in-project-switch (on-other-window invalidate-cache)
  "Jump to a project's file using completion.
With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let* ((project-root (cl-loop for i in projectile-known-projects
                             if (equal (+workspace-current-name) (doom-project-name i))
                             return i))
         (file (projectile-completing-read "Find file: "
                                           (projectile-project-files (s-replace "~" "/Users/royokong" project-root))))
         (ff (if on-other-window #'find-file-other-window #'find-file)))
    (when file
      (funcall ff (expand-file-name file project-root))
      (run-hooks 'projectile-find-file-hook))))

(defun toggle-frame-maximized-or-fullframe ()
  (interactive)
  (let ((bias (if (equal (shell-command-to-string "m1ddc display list") "")
                  37 0)))
    (set-frame-position (selected-frame) 0 bias)
    (if (equal (display-pixel-width) 4000)
        ;; for DELL U2720Q
        ;; don't know why `(display-pixel-width)' return 400 for it
        (set-frame-size (selected-frame) (- 2560 20) 1440 t)
      (set-frame-size (selected-frame)
                      (- (display-pixel-width) 20)
                      (- (display-pixel-height) bias) t))))

(defun make-second-screen-frame ()
  (interactive)
  (if (cl-find-if (lambda (frame) (equal "second-screen" (frame-parameter frame 'name)))
                      (frame-list))
      (select-frame-by-name "second-screen")
    (let ((frame (make-frame)))
      (set-frame-position frame 2560 -655)
      (set-frame-name "second-screen")
      (set-frame-size frame  (- 1440 20) (- 2560 4) t))))

(map!
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
 "s-n" #'make-second-screen-frame
 "s-q" #'save-buffers-kill-emacs
 "s-;" #'vterm-pop-posframe-toggle

 "C-0" #'toggle-frame-maximized-or-fullframe

 "C-x w" #'winner-undo

 (:leader
  ;;:desc "switch buffer" :n "j" #'+ivy/switch-buffer
  ;;:desc "switch workspace buffer" :n "k" #'+ivy/switch-workspace-buffer

  :desc "open term" :n "o t" #'vterm-pop-posframe-toggle

  :desc "translate" :n "c t" #'translate-shell
  :desc "translate rewrite" :n "c w" #'translate-rewrite

  :desc "close window" :n "w c" #'+workspace/close-window-or-workspace

  :desc "switch" :n "SPC" #'(lambda(arg)
                              (interactive "P")
                              (condition-case nil
                                  (in-project-switch nil nil)
                                (error (project-find-file))))

  :desc "search Google" :n "s o" #'(lambda(arg)
                                     (interactive (list (if (use-region-p)
                                                            (doom-thing-at-point-or-region)
                                                          (read-string "Search for: "))))
                                     (+lookup/online arg "Google"))

  :desc "music" :n "m m" #'netease-cloud-music
  :desc "eaf browser" :n "m b" #'eaf-open-browser
  :desc "eaf browser h" :n "m h" #'eaf-open-browser-with-history
  :desc "bibtex" :n "n b" #'citar-open-files
  :desc "bibtex" :n "n B" #'citar-open
  :desc "switch buffer" :n "j" #'switch-to-buffer
  :desc "switch workspace buffer" :n "k" #'+vertico/switch-workspace-buffer
  :desc "kill workspace buffer" :n "K" #'persp-kill-buffer
  :desc "Org roam find file" :n "v" #'org-roam-node-find
  :desc "toggle last popup" :n "d" #'(lambda()
                                       (interactive)
                                       (let ((closep (+popup-windows)))
                                           (+popup/toggle)
                                           (if closep
                                               (balance-windows))))

  (:prefix-map ("/" . "remote")
        :desc "switch buffer other window" :n "j" #'switch-to-buffer-other-window
        :desc "switch workspace buffer other window" :n "k" #'(lambda (arg)
                                                                (interactive "P")
                                                                (let ((consult--buffer-display  #'switch-to-buffer-other-window))
                                                                (+vertico/switch-workspace-buffer)))
        :desc "switch other window" :n "SPC" #'(lambda (arg)
                                                (interactive "P")
                                                (condition-case nil
                                                    (in-project-switch t nil)
                                                  (error (projectile-find-file-other-window))))
        :desc "close other window" :n "c" #'(lambda (arg)
                                              (interactive "P")
                                              (cl-loop for i in (window-list)
                                                    unless (or (eq i (selected-window))
                                                               (eq "*sort-tab*" (buffer-name (window-buffer i))))
                                                    return (delete-window i)))
        :desc "horizontal split" :n "/" #'split-window-horizontally)

  :desc "vertical split" :n "-" #'split-window-vertically
  :desc "ace" :n "w a" #'ace-select-window
  :desc "kill other window" :n "w 0" #'delete-other-windows)

 :nv "t" #'doctor
 (:after doctor-chatgpt
  :nv "T" #'doctor-chatgpt-gpt4)

 :nv "S" #'color-rg-search-input

 (:after cape
  "M-/" #'cape-dabbrev)

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

 (:after telega
  :map telega-chat-mode-map
  "C-c C-r" #'telega-sticker-choose-favorite-or-recent)

 (:after telega
  :map telega-msg-button-map
  "SPC" nil)

 (:after ein
  :map ein:notebook-mode-map
  "C-c b" #'ein:worksheet-insert-cell-below
  :n "RET" #'ein:worksheet-execute-cell)

 (:after imenu-list
  "C-c u" #'imenu-list-smart-toggle
  :map imenu-list-major-mode-map
  "j" #'next-line
  "k" #'previous-line)

 (:after python
  :map python-mode-map
  :n "X" #'(lambda ()
             (interactive)
             (evil-open-below 0)
             (insert "import pdb;pdb.set_trace()")
             (evil-normal-state)
             (save-buffer)))

 (:after dired
  :map dired-mode-map
  :n "h" #'dired-up-directory
  :n "l" #'dired-find-file
  "s-o" #'(lambda ()
          (interactive)
          (dired-do-shell-command
           "open" nil
           (dired-get-marked-files t current-prefix-arg))))

 (:after biblio
  :map biblio-selection-mode-map
  "j" #'biblio--selection-next
  "k" #'biblio--selection-previous
  "d" #'(lambda ()
          (interactive)
          (-if-let* ((url (biblio-get-url (biblio--selection-metadata-at-point))))
              ;; https://arxiv.org/abs/1908.10084v1 -> https://arxiv.org/pdf/1908.10084v1.pdf
              (browse-url (concat (string-replace "abs" "pdf" url) ".pdf"))
            (user-error "This record does not contain a URL"))))

 (:after symbol-overlay
  :n "N" #'symbol-overlay-put
  :n "J" #'symbol-overlay-jump-next
  :n "K" #'symbol-overlay-jump-prev)

 (:after evil
  :n "!" #'shell-command
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


(after! evil-textobj-tree-sitter
  (evil-define-key 'normal 'tree-sitter-mode
    "c" nil
    "cj" +tree-sitter-goto-next-map
    "ck" +tree-sitter-goto-previous-map)
)
