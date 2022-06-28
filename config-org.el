;;; config-org.el -*- lexical-binding: t; -*-
(use-package! org-tempo)

(after! org
  :config
  (require 'ox)
  (defun format-image-inline (source attributes info)
    (format "<img src=\"data:image/%s;base64,%s\"%s />"
            (or (file-name-extension source) "")
            (base64-encode-string
             (with-temp-buffer
               (insert-file-contents-literally source)
               (buffer-string)))
            (file-name-nondirectory source)))

  (defun org-html-export-to-mhtml (async subtree visible body)
    (cl-letf (((symbol-function 'org-html--format-image) 'format-image-inline))
      (org-html-export-to-html nil subtree visible body)))

  (org-export-define-derived-backend 'html-inline-images 'html
    :menu-entry '(?h "Export to HTML" ((?m "As MHTML file and open" org-html-export-to-mhtml))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (python . t)
     (shell . t)
     (jupyter . t)
     (sql . t)))
  (setq org-hide-emphasis-markers t)
  (setq org-babel-python-command "/opt/homebrew/bin//python3")

  ;; overide python blocks to jupyter python
  ;; (org-babel-jupyter-override-src-block "python")
  ;; no sync
  (setq ob-async-no-async-languages-alist '("python" "jupyter-python"))

  (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
                                                      (:session . "py")))

  (setq org-emphasis-alist
        (quote (
                ("/" italic)
                ("_" underline)
                ("+" (:strike-through t))
                ("~" org-verbatim verbatim)
                ("*" (:foreground "yellow" :background "black"))
                )))

  (setq org-html-text-markup-alist
        '((bold . "<mark style=\"font-style:normal;font-weight:normal\">%s</mark>")
          (code . "<code>%s</code>")
          (italic . "<i>%s</i>")
          (strike-through . "<del>%s</del>")
          (underline . "<span class=\"underline\">%s</span>")
          (verbatim . "<code>%s</code>")))
  (use-package! ob-ipython)
  (use-package! ob-async)

  (use-package! org-download
    :config
    (add-hook 'dired-mode-hook 'org-download-enable)
    (setq-default org-download-heading-lvl nil
                  org-download-image-dir "~/.org/images"
                  org-download-screenshot-method "screencapture -s %s"
                  org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory)))

  (use-package! org-capture
    :config
    (setq org-directory "~/.org")
    (setq org-default-notes-file (concat org-directory "/inbox.org")))
  )



(after! org-roam
  :config
  (use-package! websocket
    :after org-roam)
  (setq +org-roam-open-buffer-on-find-file nil))


;;(use-package! org-roam-protocol
    ;;:after org-roam ;; or :after org
    ;;:config
    ;;(setq org-roam-capture-ref-templates '(("r" "ref" plain "${body}\n"
                                            ;;:if-new
                                            ;;(file+head "${slug}.org" "#+title: ${title}")
                                            ;;:unnarrowed t)))
  ;;)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    ;; :hook
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; org roam bibtex configs
(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode))

(use-package! org-ref
  :after org-roam
  :config
  (setq org-ref-default-bibliography (list "/Users/royokong/mendeley.bib/library.bib")
        org-ref-get-pdf-filename-function #'(lambda (key)
                                            (car (bibtex-completion-find-pdf-in-field key)))
        org-ref-completion-library 'org-ref-ivy-cite))

(after! helm-bibtex
  :config
  (setq
   bibtex-completion-notes-path "/Users/royokong/.org/refs"
   bibtex-completion-bibliography "/Users/royokong/mendeley.bib/library.bib"
   bibtex-completion-pdf-field "file"
    )
(defun bibtex-completion-find-pdf-in-field (key-or-entry)
  "Return the path of the PDF specified in the field `bibtex-completion-pdf-field' if that file exists.
Returns nil if no file is specified, or if the specified file
does not exist, or if `bibtex-completion-pdf-field' is nil."
  (when bibtex-completion-pdf-field
    (let* ((entry (if (stringp key-or-entry)
                      (bibtex-completion-get-entry1 key-or-entry t)
                    key-or-entry))
           (value (bibtex-completion-get-value bibtex-completion-pdf-field entry)))
      (setq value (replace-regexp-in-string ":pdf" "" (concat "/" value)))
      (setq value (replace-regexp-in-string ":" "" value))
      (cond
       ((not value) nil)         ; Field not defined
       ((f-file? value) (list value))   ; A bare full path was found.
       ((-any 'f-file? (--map (f-join it (f-filename value)) (-flatten bibtex-completion-library-path))) (-filter 'f-file? (--map (f-join it (f-filename value)) (-flatten bibtex-completion-library-path))))
       ))))
(helm-add-action-to-source "Open PDF" 'helm-bibtex-open-pdf helm-source-bibtex 0)
)


(after! org-noter
  :config
  (load! "modified-org-noter")
)

(after! org-download
  (defun org-screenshot ()
    (interactive)
    (org-download-screenshot)
    (search-backward "DOWNLOADED" nil t)
    (end-of-line)
    (insert "\n#+attr_html: :width 700px
#+attr_latex: :width 700px")))

;;(use-package! org-protocol-capture-html
;;  :after org-capture
;;  :config
;;  (setq org-capture-templates
;;       `(("t" "Task" entry (file ,(concat org-directory "/todo.org"))
;;           "* TODO %?\n  %T\n  %a")
;;          ("w" "Web site" entry
;;           (file "")
;;           "* %a :website:\n\n%U %?\n\n%:initial"))))

(use-package! org-fragtog
  :after org
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))


(provide 'config-org)
