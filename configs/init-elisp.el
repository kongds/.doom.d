;;; configs/init-elisp.el -*- lexical-binding: t; -*-


(after! eros
  :config
  ;; eval end of current line
  (defun eros-eval-eol (eval-last-sexp-arg-internal)
    (interactive "P")
    (eros--eval-overlay
     (save-excursion
       (end-of-line)
       (eval-last-sexp eval-last-sexp-arg-internal))
     (point)))

  (evil-define-key '(normal visual) emacs-lisp-mode-map (kbd "SPC m e l") #'eros-eval-eol))
