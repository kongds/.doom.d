;;; eaf-config.el -*- lexical-binding: t; -*-

(use-package eaf
  :config
  (require 'eaf-browser)
  ;; (require 'eaf-pdf-viewer)
  (require 'eaf-evil)

  (advice-add '+lookup/online
              :override #'(lambda (query provider)
                            (interactive
                             (list (if (use-region-p) (doom-thing-at-point-or-region))
                                   "eaf"))
                            (eaf-search-it query)
                             ))

  ;; override SPC
  ;; SPC SPC to insert SPC in eaf
  (map!
   (:leader
   :desc "switch" :n "SPC" #'(lambda(arg)
                               (interactive "P")
                               (if (member (current-buffer) (eaf--get-eaf-buffers))
                                   (eaf-call-async "eval_function" eaf--buffer-id "insert_or_scroll_up_page" "SPC")
                                   (condition-case nil
                                       (in-project-switch nil nil)
                                     (error (project-find-file))))
                               )))

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
        (let ((front (shell-command-to-string "app-frontmost --name")))
          (unless (member front (list "Python\n" "python3\n"))
            (setq eaf--mac-has-focus nil)
            (setq eaf--mac-switch-to-python nil)
            (eaf--mac-replace-eaf-buffers)
            ))))
  (setq eaf--mac-switch-to-python-timer (run-with-timer 0.3 1
                                                        'eaf--mac-switch-to-python-timer-fun))


  (defun eaf--mac-replace-eaf-buffers ()
    (let ((current-window (selected-window)))
      (dolist (window (window-list))
        (select-window window)
        (when (eq major-mode 'eaf-mode)
          (get-buffer-create "*eaf temp*")
          (switch-to-buffer "*eaf temp*" t)))
      (select-window current-window)))

  ;;(cancel-timer eaf--mac-switch-to-python-timer)
  )

(provide 'eaf-config)
