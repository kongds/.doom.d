;;; config-org.el -*- lexical-binding: t; -*-

(after! org-roam
  :config
  (setq +org-roam-open-buffer-on-find-file nil))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-protocol
    :after org-roam ;; or :after org
    :config
    (setq org-roam-capture-ref-templates '(("r" "ref" plain "${body}\n"
                                            :if-new
                                            (file+head "${slug}.org" "#+title: ${title}")
                                            :unnarrowed t)))
  )

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

(provide 'config-org)
