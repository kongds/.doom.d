(require 'org-tempo)

;;(require 'ob-ipython)
;;(require 'ob-sql)
;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '((dot . t)
;;   (python . t)
;;   (shell . t)
;;   (ipython . t)
;;   (sql . t)))
;;
;;(require 'ob-async)

(setq org-startup-with-inline-images t)
(setq org-confirm-babel-evaluate nil)

(setq org-image-actual-width nil)

;;(add-hook 'org-mode-hook 'toggle-truncate-lines)

(setq org-startup-indented t)

;; use org-download to paste picture in org mode use org-downlaod-screenshot
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq-default org-download-heading-lvl nil
              org-download-image-dir "~/.org/images"
              org-download-screenshot-method "screencapture -s %s"
              org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory))

(add-hook 'org-mode-hook
	  (lambda()
	    (setq truncate-lines nil))) 

(require 'org-capture)
(add-to-list `load-path (expand-file-name "~/emacs_configs/.emacs.d/elpa/org-protocol-capture-html"))
(require 'org-protocol-capture-html)
(setq org-directory "~/.org")
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
             `(("w" "Web site" entry (file ,(concat org-directory "/web.org"))
               "* %a :website:\n\n%U %?\n\n%:initial")
               ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
		        "* TODO %?\n  %u\n  %a")
               ("m" "Memory" entry (file+headline org-default-notes-file "Memorys")
                "* %?\nEntered on %U\n  %i\n  %a")))

(provide 'init-org)
