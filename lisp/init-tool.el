(defun tool-start-remote-sync ()
  (interactive)
  (start-process-shell-command "sync"
                               (get-buffer-create "*sync*")
                               "startSync"))


(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


(require 'cheat-sh)

;; asynchronous cheat-sh
(defun cheat-sh (thing)
  "Look up THING on cheat.sh and display the result."
  (interactive "sLookup: ")
  (let* ((url-request-extra-headers `(("User-Agent" . ,cheat-sh-user-agent))))
    (url-retrieve (format cheat-sh-url (url-hexify-string thing))
                  (lambda (s)
                    (set-buffer-multibyte t)
                    (setf (point) (point-min))
                    (let ((result (when (search-forward-regexp "^$" nil t)
                                    (buffer-substring (1+ (point)) (point-max)))))
                      (if result
                          (let ((temp-buffer-window-setup-hook
                                 (cons 'cheat-sh-mode-setup temp-buffer-window-show-hook)))
                            (with-temp-buffer-window "*cheat.sh*" nil 'help-window-setup
                              (princ result)))))))
    ))

(global-set-key (kbd "s-d") 'cheat-sh)

(provide 'init-tool)
