;;; corfu.el -*- lexical-binding: t; -*-

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
  (define-key evil-ex-completion-map (kbd "(") (lambda ()
                                               (interactive)
                                               (if (= (point)  2)
                                                   (setq-local completion-at-point-functions
                                                               '(elisp-completion-at-point
                                                                 cape-keyword cape-dabbrev cape-tex cape-file
                                                                 tags-completion-at-point-function)))
                                               (insert "(")))


  (require 'corfu-doc)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

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
(use-package dabbrev
;; Swap M-/ and C-M-/
:bind (("M-/" . dabbrev-completion)
        ("C-M-/" . dabbrev-expand)))

(use-package kind-icon
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


;; disable lsp

(if t
    (use-package! lsp-bridge
      :init
      ;; (require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
      (require 'lsp-bridge-icon) ;; show icon for completion items, optional
      (require 'yasnippet)
      (yas-global-mode 1)

      (require 'corfu-info)
      (require 'corfu-history)
      (require 'lsp-bridge-icon) ;; show icons for completion items, optional
      (require 'lsp-bridge-orderless) ;; make lsp-bridge support fuzzy match, optional
      (require 'tabnine-capf)

      (defun lsp-bridge-init-hook ()
        (setq-local corfu-auto nil) ;; let lsp-bridge control when popup completion frame
        (lsp-bridge-mode 1)
        (setq-local completion-category-defaults nil)
        (setq-local completion-at-point-functions
                    (list
                     (cape-capf-buster
                      (cape-super-capf
                       #'lsp-bridge-capf
                       #'cape-dabbrev
                       ;; #'tabnine-completion-at-point
                       ;; #'cape-file
                       )
                      'equal))))

      (remove-hook 'python-mode-local-vars-hook #'lsp!) ;; disable lsp
      (remove-hook 'python-mode-local-vars-hook #'+python-init-anaconda-mode-maybe-h)
      (remove-hook 'python-mode-hook #'+python-use-correct-flycheck-executables-h)
      (add-hook 'python-mode-hook #'(lambda()
                                      (setq-local flycheck-checker 'python-pyright)))

      (add-hook 'python-mode-local-vars-hook #'lsp-bridge-init-hook))
  (use-package! lsp-mode
    :custom (lsp-completion-provider :none) ;; we use Corfu!
    :init (setq lsp-completion-enable t)
    (defun my/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))
      ;; (setq-local completion-at-point-functions
      ;;             (list  (cape-super-capf #'lsp-completion-at-point
      ;;                                     (cape-company-to-capf (apply-partially #'company--multi-backend-adapter '(company-dabbrev-code company-tabnine))))))
      (setq-local completion-at-point-functions (list  (cape-super-capf #'lsp-completion-at-point
                                                                        (cape-company-to-capf
                                                                         (apply-partially
                                                                          #'company--multi-backend-adapter
                                                                          '(company-dabbrev-code))))))) ;; Configure orderless
    :hook (lsp-completion-mode . my/lsp-mode-setup-completion)))

;; A few more useful configurations...
(use-package! emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  (setq +lsp-company-backends nil)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(corfu-history-mode t)

(provide 'corfu-company)
