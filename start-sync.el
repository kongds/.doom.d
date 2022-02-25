;;; start-sync.el -*- lexical-binding: t; -*-

(defvar startsync-running nil)
(defvar startsync-dirs nil)

(defun startsync-controller-repl-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
        (insert string))
    (when (s-contains?  "finish sync" string )
      (setq startsync-running nil)
      (force-mode-line-update)
      )
    (when (s-contains? "/Users/royo" string)
      (setq startsync-running t)
      (force-mode-line-update)
      )
    ))

(defun startsync-after-save-hook ()
  (let* ((project-root (loop for i in projectile-known-projects
                             if (equal (+workspace-current-name) (doom-project-name i))
                             return i)))
    (when (and project-root
             (member t (mapcar
                        (lambda (s)
                          (equal (replace-regexp-in-string "~" "/Users/royokong" project-root) s)
                          )
                        startsync-dirs)))
        (setq startsync-running t)
        (force-mode-line-update)
      )))

(def-modeline-var! +modeline-startsync
  '(:eval
    (if startsync-running
        "syncing   "
      "")))

(defun start-sync ()
  (interactive)
  (if (get-buffer "*startSync*")
      (kill-buffer "*startSync*"))
  (start-process "*startSync*" "*startSync*" "startSync")

  (remove-hook 'after-save-hook 'startsync-after-save-hook)
  (add-hook 'after-save-hook 'startsync-after-save-hook)

  (setq startsync-dirs
        (let ((json-data (with-current-buffer  (find-file-noselect "~/startSync.json")
                           (goto-char 0)
                           (json-parse-buffer)
                           ))
              (index 0) out)
          (while (< index (length json-data))
            (push (gethash "src" (aref json-data index) nil) out)
            (incf index)
            )
          (kill-buffer (get-buffer "startSync.json"))
          out))

  (def-modeline! :main
    '(""
      +modeline-matches
      " "
      +modeline-buffer-identification
      +modeline-position)
    `(""
      mode-line-misc-info
      +modeline-startsync
      +modeline-modes
      (vc-mode ("  "
                ,(all-the-icons-octicon "git-branch" :v-adjust 0.0)
                vc-mode " "))
      "  "
      +modeline-encoding
      (+modeline-checker ("" +modeline-checker "   "))))

  (set-modeline! :main 'default)

  (set-process-filter (get-buffer-process "*startSync*") 'startsync-controller-repl-filter))

(provide 'start-sync)
