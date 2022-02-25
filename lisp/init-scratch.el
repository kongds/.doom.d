(require 'immortal-scratch)
(require 'persistent-scratch)

(persistent-scratch-setup-default)
(add-hook 'after-init-hook 'immortal-scratch-mode)

(add-hook 'after-init-hook (lambda() (with-current-buffer "*scratch*"
                                       (emacs-lock-mode 'kill))))

(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-show-remaining-keys t)
)

(provide 'init-scratch)
