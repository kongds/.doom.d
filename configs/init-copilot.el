;;; configs/init-copilot.el -*- lexical-binding: t; -*-

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-network-proxy '(:host "127.0.0.1" :port 1087))
  (copilot-idle-delay 0.2)
  :config
  (define-key evil-insert-state-map (kbd "C-e") 'copilot-accept-completion)
  (define-key evil-insert-state-map (kbd "M-]") 'copilot-next-completion)
  (define-key evil-insert-state-map (kbd "M-[") 'copilot-previous-completion))
