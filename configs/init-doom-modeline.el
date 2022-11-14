;;; configs/init-doom-modeline.el -*- lexical-binding: t; -*-

(after! doom-modeline
  :config
  (setq doom-modeline-lsp t)
  (setq doom-modeline-height 10)
  (set-face-attribute 'mode-line nil :family "Roboto Mono Light" :height 120)
  (set-face-attribute 'mode-line-inactive nil :family "Roboto Mono Light" :height 120)
  (setq doom-modeline-project-detection 'project)
  ;;(load! "modeline-fan")
  )
