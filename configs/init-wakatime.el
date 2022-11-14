;;; configs/init-wakatime.el -*- lexical-binding: t; -*-

(use-package! wakatime-mode
  :config
  (setq wakatime-cli-path "/opt/homebrew/bin/wakatime")
  (global-wakatime-mode))
