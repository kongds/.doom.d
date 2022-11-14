;;; configs/init-rime.el -*- lexical-binding: t; -*-

(use-package! rime
  :config
  (setq default-input-method "rime")
  (setq rime-librime-root "~/.emacs.d/librime/dist")
  (setq rime-show-candidate 'posframe)
  (setq rime-cursor "˰")

  (defvar in-updating-cursor nil)
  (defvar rime-init nil)
  (defvar rime-enable nil)

  (defun rime-evil-escape-advice (orig-fun key)
    "advice for `rime-input-method' to make it work together with `evil-escape'.
  Mainly modified from `evil-escape-pre-command-hook'"
    (if rime--preedit-overlay
        ;; if `rime--preedit-overlay' is non-nil, then we are editing something, do not abort
        (apply orig-fun (list key))
      (when (featurep 'evil-escape)
        (let* (
               (fkey (elt evil-escape-key-sequence 0))
               (skey (elt evil-escape-key-sequence 1))
               (evt (read-event nil nil evil-escape-delay))
               )
          (cond
           ((and (characterp evt)
                 (or (and (char-equal key fkey) (char-equal evt skey))
                     (and evil-escape-unordered-key-sequence
                          (char-equal key skey) (char-equal evt fkey))))
            (evil-repeat-stop)
            (evil-normal-state))
           ((null evt) (apply orig-fun (list key)))
           (t
            (setq unread-command-events (append unread-command-events (list evt)))
            (apply orig-fun (list key))
            ))))))

  (advice-add 'rime-input-method :around #'rime-evil-escape-advice)

  (global-set-key (kbd "C-\\") ;;'toggle-input-method)
                  (lambda ()
                    (interactive)
                    (setq evil-default-cursor
                          (lambda ()
                            (if (or (equal (frame-parameter nil 'cursor-color) (get 'cursor 'evil-emacs-color))
                                    (equal (frame-parameter nil 'cursor-color) (get 'cursor 'evil-normal-color)))
                                (evil-set-cursor-color (if rime-enable
                                                           (get 'cursor 'evil-emacs-color)
                                                         (get 'cursor 'evil-normal-color)))
                              (+evil-update-cursor-color-h))))
                    (unless rime-init
                      (setq rime-init t)
                      (add-hook 'input-method-activate-hook (lambda ()
                                                              (setq-local rime-enable t)
                                                              (funcall evil-default-cursor)))
                      (add-hook 'input-method-deactivate-hook (lambda ()
                                                                (setq-local rime-enable nil)
                                                                (funcall evil-default-cursor))))
                    (toggle-input-method))))
