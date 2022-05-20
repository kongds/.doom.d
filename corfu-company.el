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
   ("SPC" . (lambda ()
              (interactive)
              (pcase-let ((`(,beg ,end ,table ,pred) completion-in-region--data))
                (let ((newstr (buffer-substring-no-properties beg end)))
                  (corfu-complete)
                  (if (equal newstr (buffer-substring-no-properties beg (point)))
                      (insert " ")))))))
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-auto-delay 0.1)
  :init
  (global-corfu-mode)
  (add-hook 'corfu-mode-hook (lambda ()
                               (global-company-mode -1)))

  (require 'corfu-doc)
  (define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

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
(use-package! lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-completion-enable t)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; (setq-local completion-at-point-functions
    ;;             (list  (cape-super-capf #'lsp-completion-at-point
    ;;                                     (cape-company-to-capf (apply-partially #'company--multi-backend-adapter '(company-dabbrev-code company-tabnine))))))
    (setq-local completion-at-point-functions
                (list  (cape-super-capf #'lsp-completion-at-point
                                        (cape-company-to-capf (apply-partially #'company--multi-backend-adapter '(company-dabbrev-code))))))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(if nil
    (use-package! lsp-bridge
      :init
      ;; (require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
      (require 'lsp-bridge-icon) ;; show icon for completion items, optional
      (setq lsp-completion-enable nil)
      (defun lsp-bridge-python-hook ()
        (setq-local corfu-auto nil) ;; let lsp-bridge control when popup completion frame
        (lsp-bridge-mode 1)
        (setq-local completion-at-point-functions (list (cape-super-capf #'lsp-bridge-capf
                                                                         (cape-company-to-capf
                                                                          (apply-partially
                                                                           #'company--multi-backend-adapter
                                                                           '(company-dabbrev-code company-tabnine)))))))
      (add-hook 'python-mode-hook #'lsp-bridge-python-hook)))

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



(provide 'corfu-company)
