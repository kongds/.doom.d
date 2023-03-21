;;; +patches.el -*- lexical-binding: t; -*-

(defun projectile-compilation-buffer-name (compilation-mode)
  "Meant to be used for `compilation-buffer-name-function`.
Argument COMPILATION-MODE is the name of the major mode used for the compilation buffer."
  (concat "*" (downcase compilation-mode) "*"
          (if (projectile-project-p) (concat "<" (projectile-project-name) ">") "")))

(defun run-command-with-notify (command )
after  (unless (member '("\\*Async[a-z0-9 ]*" display-buffer-no-window)  display-buffer-alist)
    (add-to-list 'display-buffer-alist
             '("\\*Async[a-z0-9 ]*" display-buffer-no-window)))
  (let* ((shell-buffer-name (concat "*Async shell command" (prin1-to-string (random 1000000)) "*"))
         (output-buffer (get-buffer-create shell-buffer-name))
         (proc (progn
                 (async-shell-command command output-buffer)
                 (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc #'(lambda (process signal)
                                       (when (memq (process-status process) '(exit signal))
                                         (kill-buffer shell-buffer-name)
                                         (shell-command-sentinel process signal)))))))

;; fix Marker does not point anywhere
(after! org-src
  (defun org-src--edit-buffer (beg end)
    "Return buffer editing area between BEG and END.
Return nil if there is no such buffer."
    (catch 'exit
      (dolist (b (buffer-list))
        (with-current-buffer b
	  (and (org-src-edit-buffer-p)
	       (eq (marker-buffer beg) (marker-buffer org-src--beg-marker))
	       (= beg org-src--beg-marker)
	       (eq (marker-buffer end) (marker-buffer org-src--end-marker))
	       (= end org-src--end-marker)
	       (throw 'exit b)))))))


(after! projectile
  (defun projectile-project-root-remove-home-dir (dir)
    (if (equal (expand-file-name "~/") dir)
        nil
      dir))
  (advice-add 'projectile-project-root :filter-return #'projectile-project-root-remove-home-dir))
