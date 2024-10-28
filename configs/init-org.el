;;; config-org.el -*- lexical-binding: t; -*-
(after! org
  :config

  (require 'org-eldoc)

  ;; (after! org-appear
  ;;   (remove-hook 'org-mode-hook #'org-appear-mode))

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

  ;; disable indent for bugs in org
  ;; (setq org-startup-indented nil)

  (setq org-hide-emphasis-markers nil)
  (setq ob-async-no-async-languages-alist '("python" "jupyter-python"))
  ;; (setq org-emphasis-alist
  ;;       (quote (
  ;;               ("/" italic)
  ;;               ;;("_" underline)
  ;;               ;;("*" (:foreground "yellow" :background "black"))
  ;;               ("+" (:strike-through t))
  ;;               ("~" org-verbatim verbatim)
  ;;               )))
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
  (when (and (version<= "29" emacs-version)
             (version< emacs-version "30"))
    (require 'emacsql-sqlite-builtin)
    (setq org-roam-database-connector 'sqlite-builtin))
  :config
  (use-package! websocket
    :after org-roam)
  (setq org-roam-completion-everywhere nil)
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

  ;; remove  the error "`org-element-at-point' cannot be used in non-Org buffer %S (%s)"
  ;; from org-element-at-point
  (defun org-element-at-point (&optional epom cached-only)
    "Determine closest element around point or EPOM.
  
  When EPOM is an element, return it immediately.
  Otherwise, determine element at EPOM marker or position.
  
  Only check cached element when CACHED-ONLY is non-nil and return nil
  unconditionally when element at EPOM is not in cache.
  
  Return value is a list like (TYPE PROPS) where TYPE is the type
  of the element and PROPS a plist of properties associated to the
  element.
  
  Possible types are defined in `org-element-all-elements'.
  Properties depend on element or object type, but always include
  `:begin', `:end', and `:post-blank' properties.
  
  As a special case, if point is at the very beginning of the first
  item in a list or sub-list, returned element will be that list
  instead of the item.  Likewise, if point is at the beginning of
  the first row of a table, returned element will be the table
  instead of the first row.
  
  When point is at the end of the buffer, return the innermost
  element ending there.
  
  This function may modify the match data."
    (if (org-element-type epom t) epom
      (setq epom (or epom (point)))
      (org-with-point-at epom
        ;; Allow re-parsing when the command can benefit from it.
        (when (and cached-only
                   (memq this-command org-element--cache-non-modifying-commands))
          (setq cached-only nil))
        (let (element)
          (when (org-element--cache-active-p)
            (if (not (org-with-base-buffer nil org-element--cache)) (org-element-cache-reset)
              (unless cached-only (org-element--cache-sync (current-buffer) epom))))
          (setq element (if cached-only
                            (when (and (org-element--cache-active-p)
                                       (or (not org-element--cache-sync-requests)
                                           (< epom
                                              (org-element--request-beg
                                               (car org-element--cache-sync-requests)))))
                              (org-element--cache-find epom))
                          (condition-case-unless-debug err
                              (org-element--parse-to epom)
                            (error
                             (org-element--cache-warn
                              "Org parser error in %s::%S. Resetting.\n The error was: %S\n Backtrace:\n%S\n Please report this to Org mode mailing list (M-x org-submit-bug-report)."
                              (buffer-name (current-buffer))
                              epom
                              err
                              (when (and (fboundp 'backtrace-get-frames)
                                         (fboundp 'backtrace-to-string))
                                (backtrace-to-string (backtrace-get-frames 'backtrace))))
                             (org-element-cache-reset)
                             (org-element--parse-to epom)))))
          (when (and (org-element--cache-active-p)
                     element
                     (org-element--cache-verify-element element))
            (setq element (org-element--parse-to epom)))
          (unless (org-element-type-p element 'org-data)
            (unless (and cached-only
                         (not (and element
                                 (or (= epom (org-element-begin element))
                                     (and (not (org-element-type-p element org-element-greater-elements))
                                          (>= epom (org-element-begin element))
                                          (< epom (org-element-end element)))
                                     (and (org-element-contents-begin element)
                                          (>= epom (org-element-begin element))
                                          (< epom (org-element-contents-begin element)))
                                     (and (not (org-element-contents-end element))
                                          (>= epom (org-element-begin element))
                                          (< epom (org-element-end element)))))))
              (if (not (org-element-type-p element 'section))
                  element
                (org-element-at-point (1+ epom) cached-only))))))))

  (after! evil-collection
    (evil-collection-define-key 'normal 'org-mode-map (kbd "C-o") 'org-babel-new-block))
  (define-key org-mode-map (kbd "C-j") #'org-babel-next-src-block)
  (define-key org-mode-map (kbd "C-k") #'org-babel-previous-src-block))

;; babel + lsp-bridge
(after! (org lsp-bridge)
  ;; support return
  (defun acm-maybe-complete ()
    (if (acm-frame-visible-p acm-menu-frame)
        (or (acm-complete) t)
      nil))
  (add-to-list 'acm-continue-commands '+org/return)
  (advice-add '+org/return :before-until #'acm-maybe-complete))


;;(after! (org warnings)
;;  ;; encounter a bug in org-mode 9.6 when using latex preview
;;  (fset #'backtrace-get-frames nil)
;;  (fset #'backtrace-to-string nil)
;;
;;  (fset 'backtrace-get-frames nil)
;;  (fset 'backtrace-to-string nil)
;;
;;  (warnings-suppress 'org-element-cache))
