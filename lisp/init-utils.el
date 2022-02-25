(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;-------------------------------------------
;;rewrite y-or-n-p (let enter can use as y)
;;-------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)
(fset 'y-or-n-p
      (byte-compile  (lambda (prompt)
                       "Ask user a \"y or n\" question.
Return t if answer is \"y\" and nil if it is \"n\".
PROMPT is the string to display to ask the question.  It should
end in a space; `y-or-n-p' adds \"(y or n) \" to it.

No confirmation of the answer is requested; a single character is
enough.  SPC also means yes, and DEL means no.

To be precise, this function translates user input into responses
by consulting the bindings in `query-replace-map'; see the
documentation of that variable for more information.  In this
case, the useful bindings are `act', `skip', `recenter',
`scroll-up', `scroll-down', and `quit'.
An `act' response means yes, and a `skip' response means no.
A `quit' response means to invoke `keyboard-quit'.
If the user enters `recenter', `scroll-up', or `scroll-down'
responses, perform the requested window recentering or scrolling
and ask again.

Under a windowing system a dialog box will be used if `last-nonmenu-event'
is nil and `use-dialog-box' is non-nil."
                       (let ((answer 'recenter)
                             (padded (lambda (prompt &optional dialog)
                                       (let ((l (length prompt)))
                                         (concat prompt
                                                 (if (or (zerop l) (eq ?\s (aref prompt (1- l))))
                                                     "" " ")
                                                 (if dialog "" "(y or n) "))))))
                         (cond
                          (noninteractive
                           (setq prompt (funcall padded prompt))
                           (let ((temp-prompt prompt))
                             (while (not (memq answer '(act skip)))
                               (let ((str (read-string temp-prompt)))
                                 (cond ((member str '("y" "Y")) (setq answer 'act))
                                       ((member str '("n" "N")) (setq answer 'skip))
                                       (t (setq temp-prompt (concat "Please answer y or n.  "
                                                                    prompt))))))))
                          ((and (display-popup-menus-p)
                                last-input-event ; not during startup
                                (listp last-nonmenu-event)
                                use-dialog-box)
                           (setq prompt (funcall padded prompt t)
                                 answer (x-popup-dialog t `(,prompt ("Yes" . act) ("No" . skip)))))
                          (t
                           (setq prompt (funcall padded prompt))
                           (while
                               (let* ((scroll-actions '(recenter scroll-up scroll-down
                                                                 scroll-other-window scroll-other-window-down))
                                      (key
                                       (let ((cursor-in-echo-area t))
                                         (when minibuffer-auto-raise
                                           (raise-frame (window-frame (minibuffer-window))))
                                         (read-key (propertize (if (memq answer scroll-actions)
                                                                   prompt
                                                                 (concat "Please answer y or n.  "
                                                                         prompt))
                                                               'face 'minibuffer-prompt)))))
                                 ;;let ret can work
                                 (setq answer (if (equal key 13)
                                                  'act
                                                (lookup-key query-replace-map (vector key) t)))
                                 (cond
                                  ((memq answer '(skip act)) nil)
                                  ((eq answer 'recenter)
                                   (recenter) t)
                                  ((eq answer 'scroll-up)
                                   (ignore-errors (scroll-up-command)) t)
                                  ((eq answer 'scroll-down)
                                   (ignore-errors (scroll-down-command)) t)
                                  ((eq answer 'scroll-other-window)
                                   (ignore-errors (scroll-other-window)) t)
                                  ((eq answer 'scroll-other-window-down)
                                   (ignore-errors (scroll-other-window-down)) t)
                                  ((or (memq answer '(exit-prefix quit)) (eq key ?\e))
                                   (signal 'quit nil) t)
                                  (t t)))
                             (ding)
                             (discard-input))))
                         (let ((ret (eq answer 'act)))
                           (unless noninteractive
                             (message "%s%c" prompt (if ret ?y ?n)))
                           ret)))))


(provide 'init-utils)
