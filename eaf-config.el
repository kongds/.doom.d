;;; eaf-config.el -*- lexical-binding: t; -*-

(use-package! eaf
  :config
  (require 'eaf-org)
  (require 'eaf-browser)
  ;; (require 'eaf-pdf-viewer)
  (require 'eaf-evil)

  (setq eaf-python-command "/opt/homebrew/bin/python3")

  ;; fix 无法登录谷歌账号
  (setq eaf-webengine-pc-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15")

  (add-hook 'eaf-browser-hook
            (lambda ()
              (define-key eaf-mode-map (kbd "n") nil)
              (define-key eaf-mode-map (kbd "p") nil)
              (define-key eaf-mode-map (kbd "n") #'eaf-py-proxy-handle_search_forward)
              (define-key eaf-mode-map (kbd "p") #'eaf-py-proxy-handle_search_backward)))

  (advice-add 'browse-url
              :override #'(lambda (url &rest args)
                            (eaf-org-open (concat "browser::" url))))


  (advice-add 'eaf--atomic-edit
              :after #'(lambda (buffer-id focus-text)
                         (evil-insert-state)
                         (insert "i")))

  (org-link-set-parameters "https"
                           :follow #'(lambda (link)
                                       (eaf-org-open (concat "browser::https:" link)))
                           :store #'eaf-org-store-link)
  (org-link-set-parameters "http"
                           :follow #'(lambda (link)
                                       (eaf-org-open (concat "browser::http:" link)))
                           :store #'eaf-org-store-link)
  (after! magit-mode
    (defun parse-url (url)
      "convert a git remote location as a HTTP URL"
      (if (string-match "^http" url) url (replace-regexp-in-string
                                          "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                          "https://\\2/\\3" url)))
    (defun magit-open-repo ()
      "open remote repo URL"
      (interactive)
      (let ((url (magit-get "remote" "origin" "url")))
        (progn (browse-url (parse-url url))
               (message "opening repo %s" url))))

    (add-hook 'magit-mode-hook
              (lambda ()
                (local-set-key (kbd "o") 'magit-open-repo))))

  ;; override SPC
  ;; SPC SPC to insert SPC in eaf
  ;; (map!
  ;;  (:leader
  ;;  :desc "switch" :n "SPC" #'(lambda(arg)
  ;;                              (interactive "P")
  ;;                              (if (member (current-buffer) (eaf--get-eaf-buffers))
  ;;                                  (eaf-call-async "eval_function" eaf--buffer-id "insert_or_scroll_up_page" "SPC")
  ;;                                  (condition-case nil
  ;;                                      (in-project-switch nil nil)
  ;;                                    (error (project-find-file)))))))

  (add-hook 'evil-normal-state-entry-hook
            (lambda ()
              (when (derived-mode-p 'eaf-mode)
                (define-key eaf-mode-map (kbd "C-w h") #'my-evil-move-left-window)
                (define-key eaf-mode-map (kbd "C-w l") #'my-evil-move-right-window)
                (define-key eaf-mode-map (kbd "C-w C-h") #'my-evil-move-left-window)
                (define-key eaf-mode-map (kbd "C-w C-l") #'my-evil-move-right-window))))

  (setq eaf-evil-leader-key "SPC")

  (setq eaf-proxy-host "127.0.0.1"
        eaf-proxy-port "1087"
        eaf-proxy-type "http")


  (defun eaf--mac-switch-to-python-timer-fun ()
    (if eaf--mac-switch-to-python
        (let ((front (shell-command-to-string "bash /Users/royokong/fontmost_app.sh")))
        ;;(let ((front (shell-command-to-string "app-frontmost --name")))
          (unless (member front (list "Python\n" "python3\n" "Emacs\n"))
            (cancel-timer eaf--mac-switch-to-python-timer)
            (setq eaf--mac-has-focus t)
            (setq eaf--mac-switch-to-python nil)
            (eaf--mac-focus-out)))))


  (defvar eaf--mac-switch-to-python-timer nil)

  (defun eaf--mac-focus-change ()
    "Manage Emacs's focus change."
    ;; remove python timer
    (if eaf--mac-switch-to-python-timer (cancel-timer eaf--mac-switch-to-python-timer))

    (if eaf--mac-safe-focus-change
        (if (executable-find "app-frontmost")
            (let ((front (shell-command-to-string "bash /Users/royokong/fontmost_app.sh")))
            ;;(let ((front (shell-command-to-string "app-frontmost --name")))
              (cond
               ((member front (list "Python\n" "python3\n"))
                (setq eaf--mac-switch-to-python t)
                (setq eaf--mac-switch-to-python-timer (run-with-timer 0.3 1
                                                                      'eaf--mac-switch-to-python-timer-fun)))
               ((string= "Emacs\n" front)
                (cond
                 (eaf--mac-switch-to-python
                  (setq eaf--mac-switch-to-python nil))
                 ((not eaf--mac-has-focus)
                  (run-with-timer 0.1 nil #'eaf--mac-focus-in))
                 (eaf--mac-has-focus
                  (eaf--mac-focus-out))))
               (t (eaf--mac-focus-out))))
          (message "Please install app-frontmost from https://pypi.org/project/mac-app-frontmost/ to make EAF works with macOS platform."))
      (setq eaf--mac-unsafe-focus-change-timer
            (unless eaf--mac-unsafe-focus-change-timer
              (run-at-time 0.06 nil
                              #'eaf--mac-unsafe-focus-change-handler)))))

  (defun eaf--mac-replace-eaf-buffers ()
    (let ((current-window (selected-window))
          (need-select-current-window nil))
      (dolist (window (window-list))
        (when (eq (with-current-buffer (window-buffer window)
                    major-mode)  'eaf-mode)
          (setq need-select-current-window t)
          (select-window window)
          (get-buffer-create "*eaf temp*")
          (switch-to-buffer "*eaf temp*" t)))
      (if need-select-current-window (select-window current-window))))

  ;; ensure focus change function has been add
  (advice-add 'eaf-restart-process :after
                  (lambda (&rest args)
                    (remove-function after-focus-change-function #'eaf--mac-focus-change)
                    (add-function :after after-focus-change-function #'eaf--mac-focus-change)))

  (after! vertico-posframe
    (defun eaf-in-eaf-buffer (&rest r)
      (let ((has-eaf-buffer nil))
        (dolist (window (window-list))
          (if (eq (with-current-buffer (window-buffer window)
                    major-mode)  'eaf-mode)
              (setq has-eaf-buffer t)))
        has-eaf-buffer))
    (advice-add #'vertico-posframe--display :before-until #'eaf-in-eaf-buffer)
    (advice-add #'vertico-posframe--setup :before-until #'eaf-in-eaf-buffer))
  ;;(cancel-timer eaf--mac-switch-to-python-timer)
  )

(provide 'eaf-config)
