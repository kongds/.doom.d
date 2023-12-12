;;; configs/init-lsp-bridge.el -*- lexical-binding: t; -*-

;;(add-to-list 'load-path "/Users/royokong/lsp-bridge-dev-copilot")
(setq acm-enable-copilot nil)
(after! copilot
  (setq acm-backend-copilot-network-proxy copilot-network-proxy))

(defun lsp-bridge-maybe-start-from-hook ()
  (unless (or (string-match "\*org-src-fontification:" (buffer-name))
              (string-match "\*Org Src" (buffer-name))
              (string-match "\*temp" (buffer-name))
              (string-match ".org-src-babel" (buffer-name))
              (string-prefix-p " markdown-code-fontification:" (buffer-name))
              (equal (buffer-name) "*Capture*"))
    (setq-local corfu-auto nil)
    (lsp-bridge-mode))

  (unless (equal major-mode 'org-mode)
    (yas-minor-mode -1))

  ;; use lsp bridge as diagnosis
  (when (member major-mode '(python-mode c-mode c++-mode sh-mode))
    (flycheck-mode -1))

  (when (member major-mode '(python-mode c-mode c++-mode))
    (setq-local +lookup-definition-functions '(lsp-bridge-find-def t)
                +lookup-implementations-functions '(lsp-bridge-find-impl t)
                +lookup-references-functions '(lsp-bridge-find-references t))))

;; use lsp bridge for following modes
(dolist (hook '(c-mode-hook
                c++-mode-hook
                python-mode-hook
                sh-mode-hook
                emacs-lisp-mode-hook
                org-mode-hook))
  (add-hook hook #'lsp-bridge-maybe-start-from-hook))

(add-hook 'tex-mode-hook #'(lambda()
                             (setq-local corfu-auto t)
                             (setq-local corfu-auto-delay 0.1)
                             (setq-local completion-at-point-functions (list (cape-super-capf #'cape-dabbrev #'cape-tex #'cape-dict #'cape-file)))))
(after! lookup
  ;; fix rtags find defination
  ;; (point-marker) cannot work with different buffer
  (defun +lookup--run-handlers (handler identifier origin)
    (doom-log "Looking up '%s' with '%s'" identifier handler)
    (condition-case-unless-debug e
        (let ((wconf (current-window-configuration))
              (window (selected-window))
              (result (condition-case-unless-debug e
                          (+lookup--run-handler handler identifier)
                        (error
                         (doom-log "Lookup handler %S threw an error: %s" handler e)
                         'fail))))
          (cond ((eq result 'fail)
                 (set-window-configuration wconf)
                 nil)
                ((or (get handler '+lookup-async)
                     (eq result 'deferred)))
                ((or result
                     (null origin)
                     (/= (point-marker) origin))
                 (prog1 (with-current-buffer (window-buffer window) (point-marker))
                   (set-window-configuration wconf)))))
      ((error user-error)
       (message "Lookup handler %S: %s" handler e)
       nil))))


(after! rtags
  ;; if rtags-imenu return No Symbols, use imenu
  (defun rtags-imenu-filter-output  (ret)
    (message "rtags-imenu-filter-output: %s" ret)
    (if (equal ret "RTags: No symbols")
      (command-execute 'imenu)  ret))
  (advice-add #'rtags-imenu :filter-return #'rtags-imenu-filter-output))

(use-package! lsp-bridge
  :commands lsp-bridge-mode
  :init
  (setq acm-enable-quick-access t)
  (setq acm-quick-access-modifier 'super)
  (setq acm-quick-access-keys '("j" "l" "f" "s" "." "g" "d" "b" "x" ","))
  ;;(setq acm-quick-access-keys '("j" "l" "3" "4" "5" "6" "7" "8" "9" ","))
  (setq acm-enable-tabnine nil)
  (setq acm-enable-yas nil)
  (setq acm-enable-preview t)

  ;;(setq acm-candidate-match-function 'orderless-flex)
  (setq acm-candidate-match-function 'regexp-quote)
  (setq acm-enable-dabbrev nil)
  (setq acm-enable-codeium t)
  (setq acm-enable-telega nil)
  (setq lsp-bridge-python-lsp-server "pyright-background-analysis")
  (setq lsp-bridge-disable-backup nil)
  (setq lsp-bridge-symbols-enable-which-func t)
  (setq lsp-bridge-enable-mode-line nil)
  (setq lsp-bridge-enable-signature-help nil)
  (setq lsp-bridge-org-babel-lang-list nil)
  (setq lsp-bridge-python-ruff-lsp-server "pyright-background-analysis_ruff")
  (setq lsp-bridge-enable-org-babel t)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq lsp-bridge-enable-inlay-hint nil)

  :config
  (add-to-list 'evil-emacs-state-modes 'lsp-bridge-ref-mode)

  (add-hook 'lsp-bridge-mode-hook #'(lambda ()
                                      (delete `(lsp-bridge-mode (" [" lsp-bridge--mode-line-format "] ")) mode-line-misc-info)))

  (remove-hook 'python-mode-local-vars-hook #'lsp!) ;; disable lsp
  (remove-hook 'python-mode-local-vars-hook #'+python-init-anaconda-mode-maybe-h)
  (remove-hook 'python-mode-hook #'+python-use-correct-flycheck-executables-h)

  (defvar lsp-bridge-running-window nil)
  (defvar lsp-bridge-running-eob-timer nil)
  (defun lsp-bridge-restart-process-try-eob ()
    (message "wait eob...")
    (if (and (window-live-p lsp-bridge-running-window)
             (equal (buffer-name (window-buffer lsp-bridge-running-window)) "*lsp-bridge*"))
        (when (> (with-current-buffer "*lsp-bridge*" (point-max))  100)
          (let ((sw (selected-window)))
            (select-window lsp-bridge-running-window)
            (recenter (- (max 1 scroll-margin)))
            (select-window sw))
          (acm-cancel-timer lsp-bridge-running-eob-timer))
      (acm-cancel-timer lsp-bridge-running-eob-timer)))

  (defun lsp-bridge-restart-process-after (&rest _)
    (when (window-live-p lsp-bridge-running-window)
      (set-window-buffer lsp-bridge-running-window (get-buffer "*lsp-bridge*"))
      (setq lsp-bridge-running-eob-timer
            (run-at-time 1 t #'lsp-bridge-restart-process-try-eob)))
    (delete-frame acm-menu-frame)
    (delete-frame acm-doc-frame))


  (after! evil-collection
    (evil-collection-define-key 'normal  'lsp-bridge-peek-mode-map
      (kbd "n") #'lsp-bridge-peek-file-content-next-line
      (kbd "p") #'lsp-bridge-peek-file-content-prev-line
      (kbd "N") #'lsp-bridge-peek-list-next-line
      (kbd "P") #'lsp-bridge-peek-list-prev-line))

  (advice-add #'lsp-bridge-restart-process
              :after #'lsp-bridge-restart-process-after)

  (advice-add #'lsp-bridge-restart-process
              :before #'(lambda (&rest _)
                          (setq lsp-bridge-running-window nil)
                          (dolist (window (window-list))
                            (when (equal (buffer-name (window-buffer window)) "*lsp-bridge*")
                              (setq lsp-bridge-running-window window)))))
  (after! org
    (advice-add #'+org/return
                :before #'(lambda ()
                            (setq lsp-org-babel-save-current--point (point)))))


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
      (r-acm-in-comment-p))))


(use-package! lsp-mode
  :commands lsp-mode
  :custom (lsp-completion-provider :none) ;; we use Corfu!
  :init (setq lsp-completion-enable t)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    (setq-local completion-at-point-functions (list  (cape-super-capf #'lsp-completion-at-point #'cape-dict #'cape-file))))
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
