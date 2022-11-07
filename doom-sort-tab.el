;;; doom-sort-tab.el -*- lexical-binding: t; -*-

(use-package! sort-tab
  :config
  (require 'consult)
  (defun sort-tab-workspace-buffer-list ()
    (cl-concatenate 'list
    (eaf--get-eaf-buffers)
    (mapcar #'get-buffer (consult--buffer-query :sort 'visibility
                                                :as #'buffer-name
                                                :predicate #'(lambda (buf)
                                                               (let* ((workspace (and t
                                                                                      (+workspace-get
                                                                                       (+workspace-current-name)
                                                                                       t))))
                                                                 (if workspace
                                                                     (+workspace-contains-buffer-p
                                                                      buf workspace) nil)))))))

  (defun sort-tab-buffer-need-hide-p (buf)
    (let* ((name (buffer-name buf)))
      (and
       (not (cl-some (lambda (prefix) (string-prefix-p prefix name)) '("*--ein")));;white list
       ;;(not (cl-some (lambda (prefix) (string-prefix-p prefix name)) '("*doom:vterm")));;white list
      (or
       (cl-some (lambda (prefix) (string-prefix-p prefix name)) '("*" " *" "COMMIT_EDITMSG"))
       (eq (aref name 0) ?\s)
       (sort-tab-is-magit-buffer-p buf)
       ))))

  (defun sort-tab-not-focus (&rest args)
    (if (eq (current-buffer) (sort-tab-get-buffer))
        (other-window 1)))


  (advice-add 'sort-tab-get-buffer-list
              :override #'(lambda ()
                            (let ((bufs (sort-tab-workspace-buffer-list)))
                              (setq bufs (cl-remove-if #'sort-tab-buffer-need-hide-p bufs))
                              (setq bufs (sort bufs #'sort-tab-buffer-freq-higher-p))
                              bufs)))

  (advice-add 'sort-tab-turn-off
              :after #'(lambda()
                         (advice-remove 'evil-window-up #'sort-tab-not-focus)
                         (advice-remove 'evil-window-down #'sort-tab-not-focus)
                         (advice-remove 'evil-window-left #'sort-tab-not-focus)
                         (advice-remove 'evil-window-right #'sort-tab-not-focus)
                         (advice-remove '+workspace-switch #'(lambda (&rest r)
                                                               (unless (get-buffer-window (sort-tab-get-buffer))
                                                                 (sort-tab-create-window))))))
  (advice-add 'sort-tab-turn-on
              :after #'(lambda()
                         (map!
                          (:leader
                           :desc "1" :n "1" #'sort-tab-select-visible-tab
                           :desc "2" :n "2" #'sort-tab-select-visible-tab
                           :desc "3" :n "3" #'sort-tab-select-visible-tab
                           :desc "4" :n "4" #'sort-tab-select-visible-tab
                           :desc "5" :n "5" #'sort-tab-select-visible-tab
                           :desc "6" :n "6" #'sort-tab-select-visible-tab
                           :desc "7" :n "7" #'sort-tab-select-visible-tab
                           :desc "8" :n "8" #'sort-tab-select-visible-tab
                           :desc "9" :n "9" #'sort-tab-select-visible-tab
                           ))
                         (advice-add 'evil-window-up :after #'sort-tab-not-focus)
                         (advice-add 'evil-window-down :after #'sort-tab-not-focus)
                         (advice-add 'evil-window-left :after #'sort-tab-not-focus)
                         (advice-add 'evil-window-right :after #'sort-tab-not-focus)
                         (advice-add '+workspace-switch
                                     :after #'(lambda (&rest r)
                                                (unless (get-buffer-window (sort-tab-get-buffer))
                                                  (sort-tab-create-window))))))
  (setq sort-tab-align 'center)
  (cl-defmacro sort-tab-update-tabs (&rest body)
  `(with-current-buffer (sort-tab-get-buffer)
     ;; Clean buffer.
     (erase-buffer)

     ;; Update tabs.
     ,@body

     (when (eq sort-tab-align 'center)
       (if (> (- (frame-text-cols) (point-max)) 0)
           (indent-line-to (/ (- (frame-text-cols) (point-max)) 2))))

     ;; Record last active buffer.
     (setq sort-tab-last-active-buffer (current-buffer))
     ))

;;(defvar sort-tab-update-list-updating nil)
(defun sort-tab-update-list ()
  (when (not (equal (window-buffer) (sort-tab-get-buffer)))
  (setq sort-tab-update-list-updating t)
  (let ((current-buffer (window-buffer)))
    (cond
     ;; Display tabs if current-buffer is normal buffer.
     ((sort-tab-is-normal-buffer-p current-buffer)
      ;; Debug usage.
      ;; (with-current-buffer (get-buffer-create "sort-tab-debug")
      ;;   (goto-char (point-max))
      ;;   (insert (format "**** %s %s\n"
      ;;                   last-command
      ;;                   (buffer-name current-buffer))))

      (let* ((current-tab-start-column 0)
             (current-tab-end-column 0)
             (tab-count 0)
             (tab-window (get-buffer-window (sort-tab-get-buffer)))
             found-current-tab
             tab)
        (sort-tab-update-tabs
         ;; Don't sort tabs if using sort-tab commands.
         (when (not (string-prefix-p "sort-tab-" (prin1-to-string last-command)))
           (setq sort-tab-visible-buffers
                  (sort-tab-get-buffer-list)))

         (setq sort-tab-visible-buffers
               (-filter
                (lambda (buf)
                    ;; filter #<killed buffer>
                    (buffer-name buf)) sort-tab-visible-buffers))

         (dolist (buf sort-tab-visible-buffers)
           ;; Insert tab.
           (setq tab (sort-tab-get-tab-name buf current-buffer))
           ;; (insert tab)
           (cl-incf tab-count)
           (if (eq buf current-buffer)
               (insert tab)
             (insert
              (concat
               (propertize
                (prin1-to-string tab-count)
                'face 'bold)
               "."
               (propertize
                (substring-no-properties tab 1)
                'face
                (if (eq (frame-parameter nil 'background-mode) 'dark)
                    '(:foreground "#bdae93" )
                  '(:foreground "#665c54"))))))
           (insert sort-tab-propertized-separator)

           ;; Calculate the current tab column.
           (unless found-current-tab
             (when (eq buf current-buffer)
               (setq found-current-tab t)
               (setq current-tab-start-column current-tab-end-column))
             (setq current-tab-end-column (+ current-tab-end-column (length tab) (length sort-tab-separator)))))

         ;; Make tab always visible.
         (when tab-window
           (with-selected-window tab-window
             (cond ((> current-tab-end-column (+ (window-hscroll) (window-width)))
                    (scroll-left (+ (- current-tab-end-column (window-hscroll) (window-width)) (/ (window-width) 2))))
                   ((< current-tab-start-column (window-hscroll))
                    (set-window-hscroll tab-window current-tab-start-column))
                   ))))))
     ;; Only display hide buffer at top if current buffer is match hide rule.
     ((sort-tab-is-hidden-buffer-p current-buffer)
      (sort-tab-update-tabs
       ;; Insert current buffer.
       (insert (sort-tab-get-tab-name current-buffer current-buffer))
       (insert sort-tab-propertized-separator)))
     ;; Erase sort-tab content if current buffer is sort-tab buffer.
     ((string-equal sort-tab-buffer-name (buffer-name current-buffer))
      (sort-tab-update-tabs))))))
  ;;(setq sort-tab-update-list-upading nil))

(defun sort-tab-get-tab-name (buf current-buffer)
  (propertize
   (format " %s "
           (let ((bufname (buffer-name buf))
                 (ellipsis "..."))
             ;; We need remove space in web page title.
             (when (sort-tab-is-eaf-browser-buffer-p buf)
               (setq bufname (replace-regexp-in-string "\\s-" "" bufname)))

             (when (cl-search ".org" bufname)
               (setq bufname (replace-regexp-in-string ".*-" "" bufname)))

             (when (cl-search "*doom:vterm" bufname)
               (setq bufname "vterm"))

             (if (> (length bufname) sort-tab-name-max-length)
                 (format "%s%s" (substring bufname 0 (- sort-tab-name-max-length (length ellipsis))) ellipsis)
               bufname)))
   'face
   (if (eq buf current-buffer)
       'sort-tab-current-tab-face
     'sort-tab-other-tab-face)))
)

(setq sort-tab-align 'center)
;;(add-hook 'doom-after-init-hook #'sort-tab-turn-on)

(setq sort-tab-name-max-length 20)

(provide 'doom-sort-tab)
