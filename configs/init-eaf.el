;;; eaf-config.el -*- lexical-binding: t; -*-

(use-package! eaf
  :commands (eaf-open eaf-open-browser eaf-open-browser-with-history)
  :config
  (require 'eaf-org)
  (require 'eaf-browser)
  ;; (require 'eaf-pdf-viewer)
  (require 'eaf-evil)

  (defun eaf--activate-emacs-mac-window()
    "Activate Emacs macOS window."
    (if eaf--topmost-has-focus
        (shell-command-to-string "open -a /Applications/Emacs.app")))

  (setq eaf-python-command "/opt/homebrew/bin/python3")

  ;; fix 无法登录谷歌账号
  (setq eaf-webengine-pc-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15")

  (add-hook 'eaf-browser-hook
            (lambda ()
              (define-key eaf-mode-map (kbd "n") nil)
              (define-key eaf-mode-map (kbd "p") nil)
              (define-key eaf-mode-map (kbd "s-c") #'(lambda()
                                                       (interactive)
                                                       (kill-new eaf--buffer-url)))
              (define-key eaf-mode-map (kbd "s-s") #'(lambda()
                                                       (interactive)
                                                       (shell-command (format "open \"%s\"" eaf--buffer-url))))
              (define-key eaf-mode-map (kbd "n") #'eaf-py-proxy-handle_search_forward)
              (define-key eaf-mode-map (kbd "p") #'eaf-py-proxy-handle_search_backward)))

  (defun eaf-browse-url  (url &rest args)
    (eaf-org-open (concat "browser::" url)))

  (advice-add 'browse-url :override #'eaf-browse-url)


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
  (after! magit
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

  ;; ensure focus change function has been add
  (advice-add 'eaf-restart-process :after
                  (lambda (&rest args)
                    (remove-function after-focus-change-function #'eaf--topmost-focus-change)
                    (add-function :after after-focus-change-function #'eaf--topmost-focus-change)))

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

