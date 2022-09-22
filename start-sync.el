;;; start-sync.el -*- lexical-binding: t; -*-

(defvar startsync-running nil)
(defvar startsync-dirs nil)
(defvar startsync-update-dir nil)

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

(defun startsync-before-save-hook ()
  (let* ((project-root (cl-loop for i in projectile-known-projects
                             if (equal (+workspace-current-name) (doom-project-name i))
                             return i))
         (project-root (if project-root (replace-regexp-in-string "~" "/Users/royokong" project-root) nil))
         (buffer-dir (buffer-file-name)))
    (setq startsync-update-dir nil)
    (when (and project-root buffer-dir
               (cl-search project-root buffer-dir)
               (member t (mapcar (lambda (s) (equal project-root  s))
                                 startsync-dirs)))
      (setq startsync-update-dir project-root)
      (shell-command "echo > ~/.sync.lock")
      (setq startsync-running t)
      (force-mode-line-update)
      )))

(defun startsync-after-save-hook ()
  (if (and startsync-update-dir
           (not (get-buffer "*StartSync*")))
      (save-window-excursion
        (let* ((shell-buffer-name "*StartSync*")
               (output-buffer (get-buffer-create shell-buffer-name))
               (proc (progn
                       (async-shell-command (concat "startSync "
                                                    " -p " startsync-update-dir) output-buffer)
                       (get-buffer-process output-buffer))))
          (if (process-live-p proc)
              (set-process-sentinel proc #'(lambda (process signal)
                                             (when (memq (process-status process) '(exit signal))
                                               (kill-buffer shell-buffer-name)
                                               (setq startsync-running nil)
                                               (force-mode-line-update)
                                               (shell-command "rm ~/.sync.lock")
                                               (shell-command-sentinel process signal))))))))
  (setq startsync-update-dir nil))

(def-modeline-var! +modeline-startsync
  '(:eval
    (if startsync-running
        "syncing   "
      "")))

(defun start-sync ()
  (interactive)
  (if (get-buffer "*startSync*")
      (kill-buffer "*startSync*"))
  ;; (start-process "*startSync*" "*startSync*" "startSync")

  (remove-hook 'before-save-hook 'startsync-before-save-hook)
  (remove-hook 'after-save-hook 'startsync-after-save-hook)
  (add-hook 'before-save-hook 'startsync-before-save-hook)
  (add-hook 'after-save-hook 'startsync-after-save-hook)

  (setq startsync-dirs
        (let ((json-data (with-current-buffer  (find-file-noselect "~/startSync.json")
                           (goto-char 0)
                           (json-parse-buffer)
                           ))
              (index 0) out)
          (while (< index (length json-data))
            (push (gethash "src" (aref json-data index) nil) out)
            (cl-incf index)
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

  ;;(set-process-filter (get-buffer-process "*startSync*") 'startsync-controller-repl-filter)
  )

(start-sync)

(provide 'start-sync)
