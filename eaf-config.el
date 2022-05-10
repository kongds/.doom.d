;;; eaf-config.el -*- lexical-binding: t; -*-

(use-package eaf
  :config
  (require 'eaf-browser)
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
  ;;(cancel-timer eaf--mac-switch-to-python-timer)


  ;; 0.75 scale
  (defun eaf-transfer-size (s)
    (round (* s  0.75)))
  (defun eaf-monitor-configuration-change (&rest _)
    "EAF function to respond when detecting a window configuration change."
    (when (and eaf--monitor-configuration-p
               (eaf-epc-live-p eaf-epc-process))
      (ignore-errors
        (let (view-infos)
          (dolist (frame (frame-list))
            (dolist (window (window-list frame))
              (with-current-buffer (window-buffer window)
                (when (derived-mode-p 'eaf-mode)
                  ;; When `eaf-fullscreen-p' is non-nil, and only the EAF window is present, use frame size
                  (if (and eaf-fullscreen-p (equal (length (cl-remove-if #'window-dedicated-p (window-list frame))) 1))
                      (push (format "%s:%s:%s:%s:%s:%s"
                                    eaf--buffer-id
                                    (eaf-get-emacs-xid frame)
                                    0 0 (eaf-transfer-size (frame-pixel-width frame))
                                    (eaf-transfer-size (frame-pixel-height frame)))
                            view-infos)
                    (let* ((window-allocation (eaf-get-window-allocation window))
                           (window-divider-right-padding (if window-divider-mode window-divider-default-right-width 0))
                           (window-divider-bottom-padding (if window-divider-mode window-divider-default-bottom-width 0))
                           (x (+ (eaf--buffer-x-position-adjust frame) (nth 0 window-allocation)))
                           (y (+ (eaf--buffer-y-postion-adjust frame) (nth 1 window-allocation)))
                           (w (nth 2 window-allocation))
                           (h (nth 3 window-allocation)))
                      (push (format "%s:%s:%s:%s:%s:%s"
                                    eaf--buffer-id
                                    (eaf-get-emacs-xid frame)
                                    (eaf-transfer-size x)
                                    (eaf-transfer-size y)
                                    (eaf-transfer-size (- w window-divider-right-padding))
                                    (eaf-transfer-size (- h window-divider-bottom-padding)))
                            view-infos)))))))
          (eaf-call-async "update_views" (mapconcat #'identity view-infos ",")))))))

(provide 'eaf-config)
