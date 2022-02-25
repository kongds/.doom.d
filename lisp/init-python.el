(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))


(add-to-list 'load-path "~/.emacs.d/elpa/ein-0.16.0")
(require 'ein)
(require 'ein-notebook)
(define-key ein:notebook-mode-map (kbd "C-c b") 'ein:worksheet-insert-cell-below)
(eval-after-load 'evil-mode
  (evil-define-key 'normal ein:notebook-mode-map (kbd "RET") 'ein:worksheet-execute-cell)
  )

(require 'lpy)
(setq evil-want-keybinding nil)
(require 'evil-collection)
(evil-collection-define-key 'normal 'python-mode-map
  "e" 'lispy-eval
  "E" 'lispy-eval-and-insert)

(add-hook 'python-mode-hook  #'(lambda ()
                                 (lpy-mode)))
(setq lpy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") 'lpy-switch-to-shell)
    (define-key map (kbd "C-c C-l") 'lpy-eval-buffer)
    map))
(setcdr
   (assq 'lpy-mode minor-mode-map-alist)
   lpy-mode-map)
(provide 'init-python)
