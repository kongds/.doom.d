;;; configs/init-corfu.el -*- lexical-binding: t; -*-

(use-package! corfu
  :bind
  ;;(:map corfu-map ("C-n" . corfu-next))
  (:map corfu-map
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ([backtab] . corfu-previous)
   ;; ("SPC" . corfu-insert-separator) use flex orderless
   ;; ("SPC" . corfu-spc)
   )

  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-auto-delay 0.1)

  :init
  (global-corfu-mode)
  (corfu-history-mode t)

  (require 'corfu-doc)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

  (add-hook 'corfu-mode-hook (lambda ()
                               (global-company-mode -1)))

  ;; (defun corfu-spec-pre-command ()
  ;;   (message (prin1-to-string completion-in-region-mode))
  ;;   (when completion-in-region-mode
  ;;     (let ((evt (read-event nil nil 0.15)))
  ;;       (when (and (characterp evt) (char-equal evt ? ))
  ;;         (message "insert SPC")
  ;;         (corfu-quit)
  ;;         (run-with-timer 0.05 nil #'(lambda() (insert " ")))
  ;;         (setq this-command 'self-insert-command)
  ;;         (setq this-orginial-command 'self-insert-command)
  ;;         ))))
  ;; (add-hook 'pre-command-hook #'corfu-spec-pre-command)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  ;; make completion work on  evil-ex

  (defun evil-ex-corfu-bracket ()
    (interactive)
    (if (= (point)  2)
        (setq-local completion-at-point-functions
                    '(elisp-completion-at-point
                      cape-keyword
                      cape-dabbrev cape-tex
                      cape-file
                      tags-completion-at-point-function)))
    (insert "("))

(defun ex-sh-completion-at-point-function ()
  (save-excursion
    (skip-chars-forward "[:alnum:]_")
    (let ((end (point))
          (_ (skip-chars-backward "[:alnum:]_"))
          (start (point)))
        (list start end #'sh--cmd-completion-table
              :company-kind
              (lambda (s)
                (cond
                 ((member s sh--completion-keywords) 'keyword)
                 ((string-suffix-p "=" s) 'variable)
                 (t 'function)))
              ))))

  (defun evil-ex-corfu-excal ()
    (interactive)
    (if (= (point)  2)
        (setq-local completion-at-point-functions
                    '(ex-sh-completion-at-point-function
                      cape-keyword
                      cape-dabbrev cape-tex
                      cape-file
                      tags-completion-at-point-function)))
    (insert "!"))

  (defun evil-ex-ret-maybe-comp ()
    (interactive)
    (message (prin1-to-string corfu--index))
    (when (>= corfu--index 0)
        (corfu--insert 'finished))
      (exit-minibuffer))

  (defun evil-ex-maybe-comp ()
    (interactive)
    (when (>= corfu--index 0)
        (corfu--insert 'finished)))

  (define-key evil-ex-completion-map (kbd "(") 'evil-ex-corfu-bracket)
  (define-key evil-ex-completion-map (kbd "!") 'evil-ex-corfu-excal)
  (define-key evil-ex-completion-map (kbd "C-e") 'evil-ex-maybe-comp)

  (defun corfu-spc ()
    (interactive)
    (pcase-let ((`(,beg ,end ,table ,pred) completion-in-region--data))
      (let ((newstr (buffer-substring-no-properties beg end)))
        (cond ((equal newstr "")
               (corfu-quit)
               (insert " "))
              (t
               (corfu-complete)
               (setq-local corfu-spc-last-beg beg
                           corfu-spc-last-end end
                           corfu-spc-last-str newstr)
               (if (equal newstr (buffer-substring-no-properties beg (point)))
                   (insert " ")))))))


  (setq evil-complete-next-func #'(lambda (arg)
                                    (corfu-next 1)))
  (setq evil-complete-previous-func #'(lambda (arg)
                                        (corfu-next -1)))
  (defun corfu--match-symbol-p (pattern sym)
    "Return non-nil if SYM is matching an element of the PATTERN list."
    (and (symbolp sym)
         (or (cl-loop for x in pattern
                      thereis (if (symbolp x)
                                  (eq sym x)
                                (string-match-p x (symbol-name sym))))
             (string-match-p "evil-complete-" (symbol-name sym))))))

;; Use dabbrev with Corfu!
(use-package! dabbrev
;; Swap M-/ and C-M-/
:bind (("M-/" . dabbrev-completion)
        ("C-M-/" . dabbrev-expand)))

(use-package! kind-icon
  ;;:ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Add extensions
(use-package! cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)
