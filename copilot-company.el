;;; copilot-company.el -*- lexical-binding: t; -*-

(use-package! copilot
  :when (featurep! :completion company)
  :config
  (setq copilot--base-dir "/Users/royokong/emacs_configs/.emacs.d.doom/.local/straight/repos/copilot.el/")
  (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))

  (defun copilot-reload-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil)))
  (defun copilot-reload-enter ()
    (interactive)
    (or (copilot-accept-completion)
        (company-complete-selection)))
  (defun copilot-reload-enter-unactive ()
    (interactive)
    (or (copilot-accept-completion)
        (newline-and-indent)))

  (map! (:when (featurep! :completion company)
         (:after company
          (:map company-mode-map
           "RET"    #'copilot-reload-enter-unactive
           [return] #'copilot-reload-enter-unactive
           "TAB"    #'copilot-reload-tab
           [tab]    #'copilot-reload-tab)
          (:map company-active-map
           "RET"    #'copilot-reload-enter
           [return] #'copilot-reload-enter
           "TAB"    #'copilot-reload-tab
           [tab]    #'copilot-reload-tab)))))

(provide 'copilot-company)
