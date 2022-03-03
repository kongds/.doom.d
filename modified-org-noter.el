;;; modified-org-noter.el -*- lexical-binding: t; -*-
(defun org-noter--create-session (ast document-property-value notes-file-path)
  (let* ((raw-value-not-empty (> (length (org-element-property :raw-value ast)) 0))
         (display-name (if raw-value-not-empty
                           (org-element-property :raw-value ast)
                         (file-name-nondirectory document-property-value)))
         (frame-name (format "Emacs Org-noter - %s" display-name))

         (document (find-file-noselect document-property-value))
         (document-path (expand-file-name document-property-value))
         (document-major-mode (buffer-local-value 'major-mode document))
         (document-buffer-name
          (generate-new-buffer-name (concat (unless raw-value-not-empty "Org-noter: ") display-name)))
         (document-buffer
          (if (eq document-major-mode 'nov-mode)
              document
            (make-indirect-buffer document document-buffer-name t)))

         (notes-buffer
          (make-indirect-buffer
           (or (buffer-base-buffer) (current-buffer))
           (generate-new-buffer-name (concat "Notes of " display-name)) t))

         (session
          (make-org-noter--session
           :id (org-noter--get-new-id)
           :display-name display-name
           :frame
             (selected-frame)
           :doc-mode document-major-mode
           :property-text document-property-value
           :notes-file-path notes-file-path
           :doc-buffer document-buffer
           :notes-buffer notes-buffer
           :level (org-element-property :level ast)
           :window-behavior (org-noter--property-or-default notes-window-behavior)
           :window-location (org-noter--property-or-default notes-window-location)
           :doc-split-fraction (org-noter--property-or-default doc-split-fraction)
           :auto-save-last-location (org-noter--property-or-default auto-save-last-location)
           :hide-other (org-noter--property-or-default hide-other)
           :closest-tipping-point (org-noter--property-or-default closest-tipping-point)
           :modified-tick -1))

         (target-location org-noter--start-location-override)
         (starting-point (point)))

    (add-hook 'delete-frame-functions 'org-noter--handle-delete-frame)
    (push session org-noter--sessions)

    (with-current-buffer document-buffer
      (cond
       ;; NOTE(nox): PDF Tools
       ((eq document-major-mode 'pdf-view-mode)
        (setq buffer-file-name document-path)
        (pdf-view-mode)
        (add-hook 'pdf-view-after-change-page-hook 'org-noter--doc-location-change-handler nil t))

       ;; NOTE(nox): DocView
       ((eq document-major-mode 'doc-view-mode)
        (setq buffer-file-name document-path)
        (doc-view-mode)
        (advice-add 'doc-view-goto-page :after 'org-noter--location-change-advice))

       ;; NOTE(nox): Nov.el
       ((eq document-major-mode 'nov-mode)
        (rename-buffer document-buffer-name)
        (advice-add 'nov-render-document :after 'org-noter--nov-scroll-handler)
        (add-hook 'window-scroll-functions 'org-noter--nov-scroll-handler nil t))

       (t (error "This document handler is not supported :/")))

      (org-noter-doc-mode 1)
      (setq org-noter--session session)
      (add-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer nil t))

    (with-current-buffer notes-buffer
      (org-noter-notes-mode 1)
      ;; NOTE(nox): This is needed because a session created in an indirect buffer would use the point of
      ;; the base buffer (as this buffer is indirect to the base!)
      (goto-char starting-point)
      (setq buffer-file-name notes-file-path
            org-noter--session session
            fringe-indicator-alist '((truncation . nil)))
      (add-hook 'kill-buffer-hook 'org-noter--handle-kill-buffer nil t)
      (add-hook 'window-scroll-functions 'org-noter--set-notes-scroll nil t)
      (org-noter--set-text-properties (org-noter--parse-root (vector notes-buffer document-property-value))
                                      (org-noter--session-id session))
      (unless target-location
        (setq target-location (org-noter--parse-location-property (org-noter--get-containing-heading t)))))

    (org-noter--setup-windows session)

    ;; NOTE(nox): This timer is for preventing reflowing too soon.
    (run-with-idle-timer
     0.05 nil
     (lambda ()
       (with-current-buffer document-buffer
         (let ((org-noter--inhibit-location-change-handler t))
           (when target-location (org-noter--doc-goto-location target-location)))
         (org-noter--doc-location-change-handler))))))


(defun org-noter (&optional arg)
  "Start `org-noter' session.

There are two modes of operation. You may create the session from:
- The Org notes file
- The document to be annotated (PDF, EPUB, ...)

- Creating the session from notes file -----------------------------------------
This will open a session for taking your notes, with indirect
buffers to the document and the notes side by side. Your current
window configuration won't be changed, because this opens in a
new frame.

You only need to run this command inside a heading (which will
hold the notes for this document). If no document path property is found,
this command will ask you for the target file.

With a prefix universal argument ARG, only check for the property
in the current heading, don't inherit from parents.

With 2 prefix universal arguments ARG, ask for a new document,
even if the current heading annotates one.

With a prefix number ARG:
- Greater than 0: Open the document like `find-file'
-     Equal to 0: Create session with `org-noter-always-create-frame' toggled
-    Less than 0: Open the folder containing the document

- Creating the session from the document ---------------------------------------
This will try to find a notes file in any of the parent folders.
The names it will search for are defined in `org-noter-default-notes-file-names'.
It will also try to find a notes file with the same name as the
document, giving it the maximum priority.

When it doesn't find anything, it will interactively ask you what
you want it to do. The target notes file must be in a parent
folder (direct or otherwise) of the document.

You may pass a prefix ARG in order to make it let you choose the
notes file, even if it finds one."
  (interactive "P")
  (cond
   ;; NOTE(nox): Creating the session from notes file
   ((eq major-mode 'org-mode)
    (when (org-before-first-heading-p)
      (user-error "`org-noter' must be issued inside a heading"))

    (let* ((notes-file-path (buffer-file-name))
           (document-property (org-noter--get-or-read-document-property (not (equal arg '(4)))
                                                                        (equal arg '(16))))
           (org-noter-always-create-frame
            (if (and (numberp arg) (= arg 0)) (not org-noter-always-create-frame) org-noter-always-create-frame))
           (ast (org-noter--parse-root (vector (current-buffer) document-property))))

      (when (catch 'should-continue
              (when (or (numberp arg) (eq arg '-))
                (cond ((> (prefix-numeric-value arg) 0)
                       (find-file document-property)
                       (throw 'should-continue nil))
                      ((< (prefix-numeric-value arg) 0)
                       (find-file (file-name-directory document-property))
                       (throw 'should-continue nil))))

              ;; NOTE(nox): Check if it is an existing session
              (let ((id (get-text-property (org-element-property :begin ast) org-noter--id-text-property))
                    session)
                (when id
                  (setq session (cl-loop for test-session in org-noter--sessions
                                         when (= (org-noter--session-id test-session) id)
                                         return test-session))
                  (when session
                    (let* ((org-noter--session session)
                           (location (org-noter--parse-location-property (org-noter--get-containing-heading))))
                      (org-noter--setup-windows session)
                      (when location (org-noter--doc-goto-location location))
                      (select-frame-set-input-focus (org-noter--session-frame session)))
                    (throw 'should-continue nil))))
              t)
        (org-noter--create-session ast document-property notes-file-path))))

   ;; NOTE(nox): Creating the session from the annotated document
   ((memq major-mode '(doc-view-mode pdf-view-mode nov-mode))
    (if (org-noter--valid-session org-noter--session)
        (progn (org-noter--setup-windows org-noter--session)
               (select-frame-set-input-focus (org-noter--session-frame org-noter--session)))

      ;; NOTE(nox): `buffer-file-truename' is a workaround for modes that delete
      ;; `buffer-file-name', and may not have the same results
      (let* ((buffer-file-name (or buffer-file-name (bound-and-true-p nov-file-name)))
             (document-path (or buffer-file-name buffer-file-truename
                                (error "This buffer does not seem to be visiting any file")))
             (document-name (file-name-nondirectory document-path))
             (document-base (file-name-base document-name))
             (document-directory (if buffer-file-name
                                     (file-name-directory buffer-file-name)
                                   (if (file-equal-p document-name buffer-file-truename)
                                       default-directory
                                     (file-name-directory buffer-file-truename))))
             ;; NOTE(nox): This is the path that is actually going to be used, and should
             ;; be the same as `buffer-file-name', but is needed for the truename workaround
             (document-used-path (expand-file-name document-name document-directory))

             (search-names (append org-noter-default-notes-file-names (list (concat document-base ".org"))))
             notes-files-annotating     ; List of files annotating document
             notes-files                ; List of found notes files (annotating or not)

             (document-location (org-noter--doc-approx-location)))

        ;; NOTE(nox): Check the search path
        (dolist (path org-noter-notes-search-path)
          (dolist (name search-names)
            (let ((file-name (expand-file-name name path)))
              (when (file-exists-p file-name)
                (push file-name notes-files)
                (when (org-noter--check-if-document-is-annotated-on-file document-path file-name)
                  (push file-name notes-files-annotating))))))

        ;; NOTE(nox): `search-names' is in reverse order, so we only need to (push ...)
        ;; and it will end up in the correct order
        (dolist (name search-names)
          (let ((directory (locate-dominating-file document-directory name))
                file)
            (when directory
              (setq file (expand-file-name name directory))
              (unless (member file notes-files) (push file notes-files))
              (when (org-noter--check-if-document-is-annotated-on-file document-path file)
                (push file notes-files-annotating)))))

        (let ((search-name (replace-regexp-in-string "[ -]" "_" (downcase document-base)))
              (node (org-roam-node-from-title-or-alias document-base))
              (findfile nil))
            ;;(dolist (file (directory-files "/Users/royokong/.org/roam" nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
              ;;(when (s-contains? search-name (replace-regexp-in-string ".*-" "" (file-name-base file)))
                ;;(message (concat "find file in roam" file))
                ;;(push (concat "/Users/royokong/.org/roam/" file) notes-files-annotating)
                ;;(setq notes-files '((concat "/Users/royokong/.org/roam/" file)))
                ;;(setq findfile t)))

            (unless node
              (org-roam-capture-
               :node (org-roam-node-create :title document-base)
               :props '(:finalize find-file))
              (org-capture-finalize)
              (insert "\n#+filetags: :paper:\n")
              (setq node (org-roam-node-from-title-or-alias document-base))
              (with-current-buffer (find-file-noselect "/Users/royokong/.org/roam/20220221212540-paper_reading.org")
                (goto-char (point-max))
                (insert (if (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$")) "" "\n")
                        "* "
                        (org-link-make-string
                         (concat "id:" (org-roam-node-id node))
                         (org-roam-node-title node)) " ")
                (org-insert-time-stamp (current-time) t)
                (save-buffer)
                ))
            (push (org-roam-node-file node) notes-files-annotating)
            (setq notes-files '((org-roam-node-file node))))

        (setq search-names (nreverse search-names))

        (when (or arg (not notes-files-annotating))
          (when (or arg (not notes-files))
            (let* ((notes-file-name (completing-read "What name do you want the notes to have? "
                                                     search-names nil t))
                   list-of-possible-targets
                   target)

              ;; NOTE(nox): Create list of targets from current path
              (catch 'break
                (let ((current-directory document-directory)
                      file-name)
                  (while t
                    (setq file-name (expand-file-name notes-file-name current-directory))
                    (when (file-exists-p file-name)
                      (setq file-name (propertize file-name 'display
                                                  (concat file-name
                                                          (propertize " -- Exists!"
                                                                      'face '(foreground-color . "green")))))
                      (push file-name list-of-possible-targets)
                      (throw 'break nil))

                    (push file-name list-of-possible-targets)

                    (when (string= current-directory
                                   (setq current-directory
                                         (file-name-directory (directory-file-name current-directory))))
                      (throw 'break nil)))))
              (setq list-of-possible-targets (nreverse list-of-possible-targets))

              ;; NOTE(nox): Create list of targets from search path
              (dolist (path org-noter-notes-search-path)
                (when (file-exists-p path)
                  (let ((file-name (expand-file-name notes-file-name path)))
                    (unless (member file-name list-of-possible-targets)
                      (when (file-exists-p file-name)
                        (setq file-name (propertize file-name 'display
                                                    (concat file-name
                                                            (propertize " -- Exists!"
                                                                        'face '(foreground-color . "green"))))))
                      (push file-name list-of-possible-targets)))))

              (setq target (completing-read "Where do you want to save it? " list-of-possible-targets
                                            nil t))
              (set-text-properties 0 (length target) nil target)
              (unless (file-exists-p target) (write-region "" nil target))

              (setq notes-files (list target))))

          (when (> (length notes-files) 1)
            (setq notes-files (list (completing-read "In which notes file should we create the heading? "
                                                     notes-files nil t))))

          (if (member (car notes-files) notes-files-annotating)
              ;; NOTE(nox): This is needed in order to override with the arg
              (setq notes-files-annotating notes-files)
            (with-current-buffer (find-file-noselect (car notes-files))
              (goto-char (point-max))
              (insert (if (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$")) "" "\n")
                      "* " document-base)
              (org-entry-put nil org-noter-property-doc-file
                             (file-relative-name document-used-path
                                                 (file-name-directory (car notes-files)))))
            (setq notes-files-annotating notes-files)))

        (when (> (length (cl-delete-duplicates notes-files-annotating :test 'equal)) 1)
          (setq notes-files-annotating (list (completing-read "Which notes file should we open? "
                                                              notes-files-annotating nil t))))

        (with-current-buffer (find-file-noselect (car notes-files-annotating))
          (goto-char (point-min))
          (unless (re-search-forward (org-re-property org-noter-property-doc-file) nil t)
            (goto-char (point-max))
            (insert (if (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$")) "" "\n")
                    "* " document-base)
            (org-entry-put nil org-noter-property-doc-file document-used-path))

          (org-with-wide-buffer
           (catch 'break
             (goto-char (point-min))
             (while (re-search-forward (org-re-property org-noter-property-doc-file) nil t)
               (when (file-equal-p (expand-file-name (match-string 3)
                                                     (file-name-directory (car notes-files-annotating)))
                                   document-path)
                 (let ((org-noter--start-location-override document-location))
                   (org-noter))
                 (throw 'break t)))))))))))

(after! evil-collection-pdf
  (evil-collection-define-key 'visual 'pdf-view-mode-map
    "h" (lambda ()
          (interactive)
          (pdf-annot-add-markup-annotation (pdf-view-active-region nil) 'highlight)
          (org-noter-insert-precise-note t)
          (pdf-view-deactivate-region)
          ;;(car (pdf-view-active-region-text))
          )
    "s"  (lambda ()
           (interactive)
           (get-paper (car (pdf-view-active-region-text)))
           (pdf-view-deactivate-region)
           )
    ))

(provide 'modified-org-noter)
