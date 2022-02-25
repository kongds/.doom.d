(define-derived-mode quick-open-mode fundamental-mode "quick-open-mode"
  (setq-local ne-highlights
	      '(
		("\/" . font-lock-function-name-face)
		("\.[^\.]*$" . font-lock-function-name-face)))
  (setq font-lock-defaults '(ne-highlights)))

(evil-define-key 'normal quick-open-mode-map (kbd "<RET>") 'quick-open-open-at-point)

(defvar *quick-open-histroy* nil)

(defun replace-all (str1 str2)
  (beginning-of-buffer)
  (replace-regexp str1 str2))

(defun quick-open-open-at-point ()
  (interactive)
  (let ((file-path
         (buffer-substring-no-properties
          (point-at-bol) (point-at-eol))))
    (kill-buffer (buffer-name))
    (find-file file-path)))

(set-popup-rule! "^\*ff:" :size 0.25 :vslot -4 :select t :quit t :ttl 0)
(defun quick-open (fileName)
  "quick open file use mdfind(only in osx)"
  (interactive
   (let ((*fileName* (read-string "file:" nil '*quick-open-histroy*)))
     (list *fileName*)))

  (let ((buffer (concat "*ff:" fileName "*")))
    (start-process buffer buffer "mdfind" "-name" fileName)
    ;;(get-buffer-create buffer)
    (with-current-buffer buffer
      ;;(shell-command-on-region (point) (point) (format "mdfind -name %s" fileName)
                               ;;(buffer-name))

      ;;(replace-all (format ".*[^/]%s.*\n" fileName) "")
      ;;(replace-all (format ".*%s.+\n" fileName) "")
      ;;(replace-all "\/Users\/royokong" "~")
      (quick-open-mode)
      (read-only-mode)
      (end-of-buffer))
    (pop-to-buffer-same-window buffer)))

(global-set-key (kbd "M-o") 'quick-open)

(provide 'init-quick-open)
