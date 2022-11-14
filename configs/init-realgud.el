;;; configs/init-realgud.el -*- lexical-binding: t; -*-

(after! realgud
  (defun realgud:file-loc-from-line-before (args)
    (mapcar #'(lambda (x)
        (if (typep x 'string)
            (replace-regexp-in-string ".mnt.jt" "/Users/royokong/nlp" x) x))
     args))
  (advice-add 'realgud:file-loc-from-line
              :filter-args 'realgud:file-loc-from-line-before)
  (advice-add 'realgud:file-column-from-string
              :filter-args 'realgud:file-loc-from-line-before)
  (advice-add 'realgud:file-line-count
              :filter-args 'realgud:file-loc-from-line-before)

  (defun realgud-send-command-before (args)
    (message (prin1-to-string args))
    (mapcar #'(lambda (x)
        (if (typep x 'string)
            (replace-regexp-in-string  ".Users.royokong.nlp" "/mnt/jt" x) x))
     args))
  (advice-add 'realgud-send-command
              :filter-args 'realgud-send-command-before)
  (defun pdb-reset ()
  "Pdb cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (pdb-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*pdb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))))))
