;;; config-org.el -*- lexical-binding: t; -*-
(after! org
  :config
  (after! lsp-bridge
    (lsp-org-babel-enable "jupyter-python"))

  (after! ox
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
      :menu-entry '(?h "Export to HTML" ((?m "As MHTML file and open" org-html-export-to-mhtml)))))

  (setq org-hide-emphasis-markers t)
  (setq ob-async-no-async-languages-alist '("python" "jupyter-python"))
  (setq org-emphasis-alist
        (quote (
                ("/" italic)
                ("_" underline)
                ("+" (:strike-through t))
                ("~" org-verbatim verbatim)
                ("*" (:foreground "yellow" :background "black")))))
  (setq org-html-text-markup-alist
        '((bold . "<mark style=\"font-style:normal;font-weight:normal\">%s</mark>")
          (code . "<code>%s</code>")
          (italic . "<i>%s</i>")
          (strike-through . "<del>%s</del>")
          (underline . "<span class=\"underline\">%s</span>")
          (verbatim . "<code>%s</code>")))
  ;; latex scale
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
  (setq org-directory "~/.org")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))


  (use-package! evil-org
    :config
    (add-hook 'org-mode-hook (lambda ()
                               (setq tab-always-indent t)
                               (evil-org-mode))))

  (use-package! ob-async)
  (use-package! org-tempo)
  (use-package! ob-jupyter)
  (use-package! org-download
    :config
    (add-hook 'dired-mode-hook 'org-download-enable)
    (setq-default org-download-heading-lvl nil
                  org-download-image-dir "~/.org/images"
                  org-download-screenshot-method "screencapture -s %s"
                  org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory))))

(defun ansi-color--find-face (codes)
  "Return the face corresponding to CODES."
  ;; Sort the codes in ascending order to guarantee that "bold" comes before
  ;; any of the colors.  This ensures that `ansi-color-bold-is-bright' is
  ;; applied correctly.
  (let (faces bright (codes (sort (copy-sequence codes) #'<)))
    (while codes
      (when-let ((face (ansi-color-get-face-1 (pop codes) bright)))
        (when (and ansi-color-bold-is-bright (eq face 'ansi-color-bold))
          (setq bright t))
        (push face faces)))
    ;; Avoid some long-lived conses in the common case.
    (if (cdr faces)
        (nreverse faces)
      (car faces))))

(after! jupyter
  :config
  ;; render color for jupyter
  (defun display-ansi-colors ()
    (ansi-color-apply-on-region (point-min) (point-max)))
  (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)
  (defun jupyter-command (&rest args)
    (with-temp-buffer
      (when (zerop (apply #'process-file "/opt/homebrew/Caskroom/miniforge/base/bin/jupyter" nil t nil args))
        (string-trim-right (buffer-string))))))

(after! org-roam
  :init
  (when (version<= "29" emacs-version)
    (require 'emacsql-sqlite-builtin)
    (setq org-roam-database-connector 'sqlite-builtin))
  :config
  (use-package! websocket
    :after org-roam)
  (setq +org-roam-open-buffer-on-find-file nil))

;; org roam bibtex configs
(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode))

(use-package! org-ref
  :after org-roam
  :config
  (setq bibtex-completion-bibliography (list "/Users/royokong/mendeley.bib/library.bib" "/Users/royokong/library.bib")
        org-ref-get-pdf-filename-function #'(lambda (key)
                                            (car (bibtex-completion-find-pdf-in-field key)))
        org-ref-completion-library 'org-ref-ivy-cite))

(after! org-download
  (defcustom org-download-screenshot-figure-size 700
    "Figure size for screenshot.")
  (setq org-download-annotate-function
    #'(lambda (link)
        (format "#+attr_html: :width %spx\n"
                org-download-screenshot-figure-size))))


(use-package! org-fragtog
  :after org
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

;; babel
(after! org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((dot . t)
                                                           (python . t)
                                                           (shell . t)
                                                           (sql . t)))

  (setq org-babel-python-command "/opt/homebrew/bin//python3")
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:kernel . "python")
                                                       (:session . "py")))

  (defun org-babel-move-to-end-block ()
    (let ((blockb "^[ \t]*#\\+BEGIN_")
          (blocke "^[ \t]*#\\+END_")
          (blockr "^[ \t]*#\\+RESULTS")
          (origin (point)) blockbp  blockep blockrp)

      ;; beginning of block
      (end-of-line)
      (if (re-search-forward blockb nil t)
          (setq blockbp (point))
        (setq blockbp (point-max)))
      (goto-char origin)

      ;; end of block
      (beginning-of-line)
      (re-search-forward blocke nil t)
      (setq blockep (point))
      (goto-char origin)

      (when (> blockbp blockep)
        ;; in block out block
        (beginning-of-line)
        (if (and (re-search-forward blockr nil t)
                 (not (next-line))
                 (< (point) blockbp))
            (while (and
                    (> (point-max) (1+ (line-beginning-position)))
                    (cl-search (buffer-substring-no-properties
                                (line-beginning-position)
                                (1+ (line-beginning-position)))
                               ":#"))
              (next-line))
          (goto-char blockep)
          (next-line)))))

  (defun org-babel-new-block ()
    (interactive)
    (let ((origin (point))
          (head nil))
      (when (re-search-backward "^[ \t]*#\\+BEGIN_SRC" nil t)
        (setq head
              (buffer-substring-no-properties
               (line-beginning-position)
               (line-beginning-position 2)))
        (goto-char origin)
        (org-babel-move-to-end-block)
        (insert (concat "\n" head))
        (insert "\n\n#+end_src\n")
        (previous-line 3))))

  (after! evil-collection
    (evil-collection-define-key 'normal 'org-mode-map (kbd "C-o") 'org-babel-new-block))
  (define-key org-mode-map (kbd "C-j") #'org-babel-next-src-block)
  (define-key org-mode-map (kbd "C-k") #'org-babel-previous-src-block))

