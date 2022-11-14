;;; configs/init-color-rg.el -*- lexical-binding: t; -*-

(use-package! color-rg
  :config
  (add-to-list 'evil-emacs-state-modes 'color-rg-search-mode)
  (add-to-list 'evil-emacs-state-modes 'color-rg-mode)

  (setq color-rg-recenter-match-line t)

  (evil-define-key 'visual evil-surround-mode-map (kbd "S") nil)
  (map! :nv "S" #'color-rg-search-input))
