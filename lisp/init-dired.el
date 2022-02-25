(require 'guide-key)
(require-package 'dired+)
(require-package 'dired-sort)
(require 'dired-rainbow)
(require 'all-the-icons-dired)
(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))



(when (maybe-require-package 'diff-hl)
  (after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(defconst my-dired-media-files-extensions
  '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg")
  "Media files.")

(dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
(dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)
(dired-rainbow-define el     "#ff00ff" ("el"))

(dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")

(define-key dired-mode-map (kbd "s-o") (lambda () (interactive) (dired-do-shell-command
                                                            "open" nil
                                                            (dired-get-marked-files t current-prefix-arg))))
(define-key dired-mode-map (kbd "SPC") 'counsel-M-x)
(define-key dired-mode-map (kbd "G") 'evil-goto-line)

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  ;;(define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "n") nil)
  (setq dired-mode-hook nil)
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%")
              (cua-mode -1)))
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook 'dired-mode-hook 'diredp--set-up-font-locking)
  (defun dired (dirname &optional switches)
    "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Optional second argument SWITCHES specifies the `ls' options used.
\(Interactively, use a prefix argument to be able to specify SWITCHES.)

If DIRNAME is a string, Dired displays a list of files in DIRNAME (which
may also have shell wildcards appended to select certain files).

If DIRNAME is a cons, its first element is taken as the directory name
and the rest as an explicit list of files to make directory entries for.
In this case, SWITCHES are applied to each of the files separately, and
therefore switches that control the order of the files in the produced
listing have no effect.

\\<dired-mode-map>\
You can flag files for deletion with \\[dired-flag-file-deletion] and then
delete them by typing \\[dired-do-flagged-delete].
Type \\[describe-mode] after entering Dired for more info.

If DIRNAME is already in a Dired buffer, that buffer is used without refresh."
     ;;Cannot use (interactive "D") because of wildcards.
    (interactive (dired-read-dir-and-switches ""))
    (if (file-directory-p dirname)
        (switch-to-buffer (dired-noselect dirname switches))
      (find-file dirname)))
	)

(after-load 'dired+
  (setq diredp-dir-name dired-directory-face))

(global-set-key (kbd "C-c p") '(lambda () (interactive)
                                 (kill-new (substring  (pwd) 10 nil))))


(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(provide 'init-dired)
