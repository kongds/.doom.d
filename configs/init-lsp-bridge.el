;;; configs/init-lsp-bridge.el -*- lexical-binding: t; -*-

;;(add-to-list 'load-path "/Users/royokong/lsp-bridge")

(add-hook 'org-mode-hook #'(lambda()
                             (setq-local corfu-auto t)
                             ;;(setq-local acm-enable-english-helper t)
                             ;;(lsp-bridge-mode)
                             (setq-local completion-at-point-functions (list  (cape-super-capf  #'cape-dict #'cape-dabbrev)))
                             ;; (setq-local +lsp-company-backends '(company-capf company-dabbrev-code company-ispell :separate))
                             ))

(add-hook 'tex-mode-hook #'(lambda()
                             (setq-local corfu-auto t)
                             ;;(setq-local acm-enable-english-helper t)
                             ;;(lsp-bridge-mode)
                             (setq-local corfu-auto-delay 0.1)
                             (setq-local completion-at-point-functions (list (cape-super-capf #'cape-dabbrev #'cape-tex #'cape-dict #'cape-file)))
                             ;; (setq-local +lsp-company-backends '(company-capf company-dabbrev-code company-ispell :separate))
                             ))

(add-hook 'emacs-lisp-mode-hook #'(lambda()
                                    (setq-local corfu-auto nil)
                                    (lsp-bridge-mode)
                                    ))

(add-hook 'python-mode-hook #'(lambda()
                                ;; remove lsp-bridge modeline
                                (setq-local corfu-auto nil)
                                (flycheck-mode -1)
                                (setq-local lsp-bridge-enable-hover-diagnostic t)

                                (lsp-bridge-mode)
                                (setq-local +lookup-definition-functions '(lsp-bridge-find-def t)
                                            +lookup-implementations-functions '(lsp-bridge-find-impl t)
                                            +lookup-references-functions '(lsp-bridge-find-references t))))


(use-package! lsp-bridge
  :commands lsp-bridge-mode
  :init
  (setq acm-enable-quick-access t)
  (setq acm-quick-access-modifier 'super)
  (setq acm-quick-access-keys '("j" "l" "f" "s" "." "g" "d" "b" "x" ","))
  (setq acm-enable-tabnine nil)
  (setq acm-enable-yas nil)
  (setq acm-candidate-match-function 'orderless-flex)
  (setq acm-enable-dabbrev nil)
  :config
  (add-to-list 'evil-emacs-state-modes 'lsp-bridge-ref-mode)

  ;;(global-lsp-bridge-mode)

  (add-hook 'lsp-bridge-mode-hook #'(lambda ()
                                      (delete `(lsp-bridge-mode (" [" lsp-bridge--mode-line-format "] ")) mode-line-misc-info)))

  (remove-hook 'python-mode-local-vars-hook #'lsp!) ;; disable lsp
  (remove-hook 'python-mode-local-vars-hook #'+python-init-anaconda-mode-maybe-h)
  (remove-hook 'python-mode-hook #'+python-use-correct-flycheck-executables-h)

  (setq lsp-bridge-disable-backup nil)

  (map!
   (:leader
    :desc "workspace symbol"
    :n "s n" #'lsp-bridge-list-workspace-symbols)))


(use-package! acm
  :after lsp-bridge
  :config
  ;; bindings
  (define-key acm-mode-map (kbd "TAB") 'acm-select-next)
  (define-key acm-mode-map (kbd "S-TAB") 'acm-select-prev)
  (define-key acm-mode-map (kbd "C-n") 'acm-select-next)
  (define-key acm-mode-map (kbd "C-p") 'acm-select-prev)

  ;; beginning-of-defun in this function slow down the lsp-bridge
  (fset 'r-acm-in-string-p (symbol-function 'acm-in-string-p))
  (fset 'r-acm-in-comment-p (symbol-function 'acm-in-comment-p))
  (defun acm-in-string-p (&optional state)
    (if (eq major-mode 'python-mode)
        nil
      (r-acm-in-string-p)))
  (defun acm-in-comment-p (&optional state)
    (if (eq major-mode 'python-mode)
        nil
      (r-acm-in-comment-p)))



  ;; timer doc
  (fset 'r-acm-update (symbol-function 'acm-update))
  (fset 'r-acm-doc-try-show (symbol-function 'acm-doc-try-show))

  (defvar acm-update-timer nil)
  (defvar acm-doc-timer nil)
  (defvar acm-delay 0.5)

  ;;(defun acm-update ()
  ;;  (if acm-update-timer
  ;;      (cancel-timer acm-update-timer))
  ;;  (setq acm-update-timer (run-with-idle-timer acm-delay nil #'r-acm-update)))

  (advice-add 'acm-doc-hide :before #'(lambda ()
                                        (if acm-doc-timer
                                            (cancel-timer acm-doc-timer))))

  (defun acm-doc-try-show ()
    (if acm-doc-timer
        (cancel-timer acm-doc-timer))
    (setq acm-doc-timer (run-with-idle-timer acm-delay nil #'r-acm-doc-try-show))))

(use-package! lsp-mode
  :commands lsp-mode
  :custom (lsp-completion-provider :none) ;; we use Corfu!
  :init (setq lsp-completion-enable t)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    (setq-local completion-at-point-functions (list  (cape-super-capf #'lsp-completion-at-point #'cape-dict #'cape-file))))
    ;; (setq-local completion-at-point-functions
    ;;             (list  (cape-super-capf #'lsp-completion-at-point
    ;;                                     (cape-company-to-capf (apply-partially #'company--multi-backend-adapter '(company-dabbrev-code company-tabnine))))))
    ;; (setq-local completion-at-point-functions (list  (cape-super-capf #'lsp-completion-at-point
    ;;                                                                   (cape-company-to-capf
    ;;                                                                    (apply-partially
    ;;                                                                     #'company--multi-backend-adapter
    ;;                                                                     '(company-dabbrev-code))))))) ;; Configure orderless
  :hook (lsp-completion-mode . my/lsp-mode-setup-completion))

;; A few more useful configurations...
(after! emacs
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
