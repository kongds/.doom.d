;;; configs/init-ein-notebook.el -*- lexical-binding: t; -*-

(after! ein-notebook
  (ein:notebook--define-key  ein:notebook-mode-map (kbd "C-o o") ein:worksheet-insert-cell-below)
  (ein:notebook--define-key  ein:notebook-mode-map (kbd "C-o d") ein:worksheet-kill-cell)
  (ein:notebook--define-key  ein:notebook-mode-map (kbd "C-o O") ein:worksheet-insert-cell-above)
  (ein:notebook--define-key  ein:notebook-mode-map (kbd "C-j") ein:worksheet-goto-next-input)
  (ein:notebook--define-key  ein:notebook-mode-map (kbd "C-k") ein:worksheet-goto-prev-input)

  (defun ein-mode-hooks ()
    (make-local-variable 'evil-motion-state-map)
    (setq-local evil-motion-state-map (copy-tree evil-motion-state-map))
    (define-key  evil-motion-state-map (kbd "C-o") nil)
    (define-key  evil-motion-state-map (kbd "C-j") nil))
  (add-hook 'ein:notebook-mode-hook 'ein-mode-hooks))
