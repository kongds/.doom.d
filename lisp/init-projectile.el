(use-package projectile
    :ensure t
    :config
    (define-key projectile-mode-map (kbd "s-f") 'projectile-command-map)
    (projectile-mode +1))
(setq projectile-completion-system 'ivy)
(setq projectile-project-root-files #'( ".projectile" ))
(setq projectile-project-root-files-functions #'(projectile-root-top-down
                                                 projectile-root-top-down-recurring
                                                 projectile-root-bottom-up
                                                 projectile-root-local))

;; use recentf-mode to projectile-recentf
(recentf-mode 1)


(provide 'init-projectile)

