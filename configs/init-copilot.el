;;; configs/init-copilot.el -*- lexical-binding: t; -*-

(use-package! copilot
  :commands copilot-mode
  :hook ((prog-mode . copilot-mode)
         (org-mode . copilot-mode))
  :custom
  ;;(copilot-network-proxy '(:host "127.0.0.1" :port 1087))
  (copilot-network-proxy nil)
  (copilot-idle-delay 0)
  (copilot-max-char 10000000)
  :config
  (define-key evil-insert-state-map (kbd "C-e") 'copilot-accept-completion)
  (define-key evil-insert-state-map (kbd "M-]") 'copilot-next-completion)
  (define-key evil-insert-state-map (kbd "M-[") 'copilot-previous-completion)
  )
