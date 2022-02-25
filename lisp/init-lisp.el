(require 'rainbow-delimiters)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; clean lispy-mode-map
            (setcdr
             (assq 'lispy-mode minor-mode-map-alist)
             (make-sparse-keymap))
            (lispy-mode)))

(show-paren-mode 1)

(setq evil-want-keybinding nil)
(require 'evil-collection)

(require 'lispy)
(evil-collection-define-key 'normal 'emacs-lisp-mode-map
  "e" 'lispy-eval
  "E" 'lispy-eval-and-insert)

(provide 'init-lisp)
