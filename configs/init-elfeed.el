;;; elfeed-arxiv.el -*- lexical-binding: t; -*-

(use-package! citar
  ;; load after citar-open-files
  :commands citar-open-files

  :hook (doom-after-modules-init . citar-refresh)

  :config
  ;; This will add watches for the global bib files and in addition add a hook to LaTeX-mode-hook and org-mode-hook to add watches for local bibliographic files.

  ;; citar for mendeley
  (defun citar-file--parser-mendeley (file-field)
    "Split FILE-FIELD by `;'."
    (seq-remove
     #'string-empty-p
     (mapcar
      (lambda (x)
        (let* ((x (string-trim x))
               (x (replace-regexp-in-string ":pdf" "" x))
               (x (replace-regexp-in-string ":Users" "/Users" x)))
          x)
        )
      (citar-file--split-escaped-string file-field ?\;))))

  (setq citar-file-parser-functions
        '(citar-file--parser-mendeley))

  (setq
   citar-bibliography (list "/Users/royokong/mendeley.bib/library.bib" "/Users/royokong/library.bib")
   citar-file-extensions '("pdf" "org" "md")
   citar-file-open-function #'find-file)
  (defun robo/citar-full-names (names)
    "Transform names like LastName, FirstName to FirstName LastName."
    (when (stringp names)
      (mapconcat
       (lambda (name)
         (if (eq 1 (length name))
             (split-string name " ")
           (let ((split-name (split-string name ", ")))
             (cl-concatenate 'string (nth 1 split-name) " " (nth 0 split-name)))))
       (split-string names " and ") ", ")))
  (setq citar-display-transform-functions
        '((("author" "editor") . robo/citar-full-names)))
  (setq citar-templates
        '((main . "${author editor:55}     ${date year issued:4}     ${title:55}")
          (suffix . "  ${tags keywords keywords:40}")
          (preview . "${author editor} ${title}, ${journal publisher container-title collection-title booktitle} ${volume} (${year issued date}).\n")
          (note . "#+title: Notes on ${author editor}, ${title}")))
  ;; use consult-completing-read for enhanced interface
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))

(use-package! elfeed
  :commands elfeed

  :config
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)

  ;; add tags of add time

  (advice-add 'elfeed-update :before
              (lambda (&rest _)
                (setq elfeed-new-entry-hook nil)
                (add-hook 'elfeed-new-entry-hook
                          (elfeed-make-tagger  :add (intern (setq date (format-time-string "add-%Y-%m-%d")))))
                (elfeed-search-clear-filter)
                (elfeed-search-set-filter (concat elfeed-search-filter
                                                  (format-time-string " +add-%Y-%m-%d")))))

  (setq elfeed-feeds '("http://export.arxiv.org/api/query?search_query=cat:cs.CL&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending"))

  (setq elfeed-search-title-max-width 130)

  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors
        concatenated."
    (mapconcat
     (lambda (author)
       (plist-get author
                  :name))
     authors-list ", "))


  (defun my-search-print-fn (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry
                                   :title)
                      (elfeed-entry-title entry)
                      ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed (or (elfeed-meta feed
                                                   :title)
                                      (elfeed-feed-title feed))))
           (entry-authors (concatenate-authors (elfeed-meta entry
                                                            :authors)))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat
                      (lambda (s)
                        (propertize s 'face 'elfeed-search-tag-face))
                      tags ","))
           (title-width (- (window-width) 10 elfeed-search-trailing-width))
           (title-column (elfeed-format-column title (elfeed-clamp elfeed-search-title-min-width
                                                                   title-width
                                                                   elfeed-search-title-max-width)
                                               :left))

           (entry-comment  (elfeed-meta entry :comment))
           (comment-width 30)
           (comment-column (elfeed-format-column (string-replace "\n" "" (or entry-comment "")) (elfeed-clamp elfeed-search-title-min-width
                                                                                     comment-width
                                                                                     elfeed-search-title-max-width)
                                                 :left))

           (entry-cat
            (s-join "," (elfeed-meta entry :categories)))
           (cat-width 10)
           (cat-column (elfeed-format-column (or entry-cat "") (elfeed-clamp elfeed-search-title-min-width
                                                                             cat-width
                                                                             elfeed-search-title-max-width)
                                                 :left))


           (authors-width 80)
           (authors-column (elfeed-format-column entry-authors (elfeed-clamp
                                                                elfeed-search-title-min-width
                                                                authors-width 131)
                                                 :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
      (insert (propertize authors-column 'face 'elfeed-search-date-face 'kbd-help entry-authors) " ")

      (insert (propertize comment-column 'face title-faces) " ")

      (insert (propertize cat-column 'face title-faces) " ")))


  (advice-add 'arxiv-get-pdf :before-until (lambda (arxiv-number pdf)
                                             (file-exists-p pdf)))

  (setq elfeed-search-print-entry-function #'my-search-print-fn)

  (setq org-ref-clean-bibtex-entry-hook '())

  (require 'org-ref)
  (require 'org-ref-arxiv)

  (defun arxiv-get-pdf-controller-repl-filter (proc string)
    (setq string (string-replace "\n" "" string))
    (message "%s" string)
    (when-let ((str-index (string-match "Opening: " string)))
      (require 'citar)
      (let* ((string (substring string str-index))
             (status (nth 2 (split-string string " ")))
             (pdf (nth 1 (split-string string " ")))
             (arxiv-number (string-replace ".pdf" ""
                                           (nth 0 (last (split-string pdf "/")))))
             (bibfile (nth 1 citar-bibliography))
             (title nil))
        (when (equal status "new")
          (save-window-excursion
            (find-file bibfile)
            (goto-char (point-max))
            (when (not (looking-at "^")) (insert "\n"))
            (insert (arxiv-get-bibtex-entry-via-arxiv-api arxiv-number))
            (goto-char (point-max))
            (when (not (looking-at "^")) (insert "\n"))
            (bibtex-end-of-entry)
            (backward-char)
            (insert (format "  file = {%s}\n  " pdf))
            (save-buffer)
            ;; get title
            (goto-char (point-max))
            (bibtex-beginning-of-entry)
            (setq title (replace-regexp-in-string
                         "{\\(.*\\)}" "\\1"
                         (string-replace "\n"  "" (cdr (assoc "title" (bibtex-parse-entry))))))
            (kill-buffer))

          ;; link file to make pdf file has title as name
          (shell-command (format "mv %s \"%s\"" pdf (string-replace arxiv-number title pdf)))
          (shell-command (format "ln -s \"%s\" %s" (string-replace arxiv-number title pdf) pdf)))

        (find-file pdf))
        ;;(start-process "*fbib*" "*fbib*" "rebiber" "-i" bibfile)
        ))


  (defun elfeed-show-open-pdf ()
    (interactive)
    (let* ((url (elfeed-entry-link elfeed-show-entry)))
      (start-process "*get_from_arxiv*" "*get_from_arxiv*" "get_from_arxiv" url)
      (set-process-filter (get-buffer-process "*get_from_arxiv*") 'arxiv-get-pdf-controller-repl-filter)))

  (defun elfeed-search-open-pdf (entry)
    (interactive (list (elfeed-search-selected :ignore-region)))
    (let* ((url (elfeed-entry-link entry)))
          (start-process "*get_from_arxiv*" "*get_from_arxiv*" "get_from_arxiv" url)
          (set-process-filter (get-buffer-process "*get_from_arxiv*") 'arxiv-get-pdf-controller-repl-filter)))

  (map!
   :map elfeed-search-mode-map :n "o" #'elfeed-search-open-pdf)
  (map!
   :map elfeed-show-mode-map :n "o" #'elfeed-show-open-pdf)


  ;; make *elfeed-xx* as real buffer
  (add-hook 'elfeed-show-mode-hook (lambda ()
                                     (+zen/toggle)
                                     (setq-local doom-real-buffer-p t)))

  (add-hook 'elfeed-search-mode-hook (lambda ()
                                       (setq-local doom-real-buffer-p t)))
)

(use-package! org-roam-bibtex
  :after citar
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ;; bibliography note template
          ("r" "bibliography reference" plain "%?"
           :if-new (file+head "references/notes_${citekey}.org"
                              "#+title: Notes on ${title}\n#+SETUPFILE: ~/bib-lib/ref_setup_file.org\n* References :ignore:\n#+print_bibliography:")
           :unnarrowed t)
          ;; for my annotated bibliography needs
          ("s" "short bibliography reference (no id)" entry "* ${title} [cite:@%^{citekey}]\n%?"
           :target (node "68d557a8-a918-4372-a930-9ee708feec35")
           :unnarrowed t
           :empty-lines-before 1
           :prepend t)))
  (defun robo/capture-to-inbox ()
    "Capture a TODO straight to the inbox."
    (interactive)
    (org-roam-capture- :goto nil
                       :keys "s"
                       :node (org-roam-node-from-id "68d557a8-a918-4372-a930-9ee708feec35")))
  (setq citar-open-note-function 'orb-citar-edit-note
        citar-notes-paths '("~/.org/roam/references/")
        orb-preformat-keywords '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf")))

(use-package! oc
  :after citar
  :config
  (require 'oc-biblatex)
  (require 'oc-csl)
  (require 'citar)
  (setq org-cite-global-bibliography citar-bibliography
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-export-processors '((latex biblatex)
                                     (t csl))))


