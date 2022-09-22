;;; copilot-company.el -*- lexical-binding: t; -*-

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-idle-delay 0.2)
  (define-key evil-insert-state-map (kbd "C-e") 'copilot-accept-completion)
  (define-key evil-insert-state-map (kbd "M-]") 'copilot-next-completion)
  (define-key evil-insert-state-map (kbd "M-[") 'copilot-previous-completion))

(provide 'copilot-company)
