(require 'pretty-hydra)
(require 'bind-key)
(provide 'init-lsp)

;;50mb
(setq gc-cons-threshold 50000000)

(defcustom centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes.
"
  :group 'centaur
  :type '(repeat (symbol :tag "Major-Mode")))

  (use-package lsp-mode
     :defines (lsp-clients-python-library-directories
               lsp-rust-server)
     :commands (lsp-enable-which-key-integration
                lsp-format-buffer
                lsp-organize-imports
                lsp-install-server)
     :diminish
     :hook ((prog-mode . (lambda ()
                           (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                             (lsp-deferred))))
            (lsp-mode . (lambda ()
                          ;; Integrate `which-key'
                          (lsp-enable-which-key-integration)

                          ;; Format and organize imports
                          (unless (apply #'derived-mode-p centaur-lsp-format-on-save-ignore-modes)
                            (add-hook 'before-save-hook #'lsp-format-buffer t t)
                            (add-hook 'before-save-hook #'lsp-organize-imports t t)))))
     :bind (:map lsp-mode-map
            ("C-c C-d" . lsp-describe-thing-at-point)
            ([remap xref-find-definitions] . lsp-find-definition)
            ([remap xref-find-references] . lsp-find-references))
     :init
     ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
     (setq-default lsp-session-file "/dev/null")
     (setq read-process-output-max (* 1024 1024)) ;; 1MB

     (setq lsp-keymap-prefix "C-c l"
           lsp-keep-workspace-alive nil
           lsp-signature-auto-activate nil
           lsp-modeline-code-actions-enable nil
           lsp-modeline-diagnostics-enable nil

           lsp-enable-file-watchers nil
           lsp-enable-folding nil
           lsp-enable-semantic-highlighting nil
           lsp-enable-symbol-highlighting nil
           lsp-enable-text-document-color nil

           lsp-enable-indentation nil
           lsp-enable-on-type-formatting nil)

     ;; For `lsp-clients'
     (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
     (when (executable-find "rust-analyzer")
       (setq lsp-rust-server 'rust-analyzer))
     :config
     (with-no-warnings
       (defun my-lsp--init-if-visible (func &rest args)
         "Not enabling lsp in `git-timemachine-mode'."
         (unless (bound-and-true-p git-timemachine-mode)
           (apply func args)))
       (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible))

     (defun lsp-update-server ()
       "Update LSP server."
       (interactive)
       ;; Equals to `C-u M-x lsp-install-server'
       (lsp-install-server t)))


(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :pretty-hydra
  ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket")
           :color amaranth :quit-key "q")
   ("Doc"
    (("d e" (progn
              (lsp-ui-doc-enable (not lsp-ui-doc-mode))
              (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
      "enable" :toggle lsp-ui-doc-mode)
     ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
      "signature" :toggle lsp-ui-doc-include-signature)
     ("d t" (setq lsp-ui-doc-position 'top)
      "top" :toggle (eq lsp-ui-doc-position 'top))
     ("d b" (setq lsp-ui-doc-position 'bottom)
      "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
     ("d p" (setq lsp-ui-doc-position 'at-point)
      "at point" :toggle (eq lsp-ui-doc-position 'at-point))
     ("d f" (setq lsp-ui-doc-alignment 'frame)
      "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
     ("d w" (setq lsp-ui-doc-alignment 'window)
      "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
    "Sideline"
    (("s e" (progn
              (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
              (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
      "enable" :toggle lsp-ui-sideline-mode)
     ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
      "hover" :toggle lsp-ui-sideline-show-hover)
     ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
      "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
     ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
      "symbol" :toggle lsp-ui-sideline-show-symbol)
     ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
      "code actions" :toggle lsp-ui-sideline-show-code-actions)
     ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
      "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
    "Action"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→")
     ("C-a" mwim-beginning-of-code-or-line nil)
     ("C-e" mwim-end-of-code-or-line nil)
     ("C-b" backward-char nil)
     ("C-n" next-line nil)
     ("C-p" previous-line nil)
     ("C-f" forward-char nil)
     ("M-b" backward-word nil)
     ("M-f" forward-word nil)
     ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
  :bind (("C-c C-u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-<f6>" . lsp-ui-hydra/body)
         ("M-RET" . lsp-ui-sideline-apply-code-actions))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-sideline-show-diagnostics nil
              lsp-ui-sideline-ignore-duplicate t
              lsp-ui-doc-position 'at-point
              lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
              lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                    ,(face-foreground 'font-lock-string-face)
                                    ,(face-foreground 'font-lock-constant-face)
                                    ,(face-foreground 'font-lock-variable-name-face))))
(require 'lsp-ui)
(require 'lsp-ui-peek)
(define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "M-,") #'lsp-ui-peek-find-references)
(define-key lsp-ui-mode-map (kbd "M->") #'lsp-ui-doc-focus-frame)
;;(define-key lsp-ui-doc-frame-mode-map (kbd "M->") #'lsp-ui-doc-unfocus-frame)
;;(require 'lsp-ui)
;;(after-load 'lsp-ui
;;	        (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;	        (define-key lsp-ui-mode-map (kbd "M-,") #'lsp-ui-peek-find-references)
;;            (defun lsp-ui-doc--display (symbol string)
;;              "Display the documentation."
;;              (when (and lsp-ui-doc-use-webkit (not (featurep 'xwidget-internal)))
;;                (setq lsp-ui-doc-use-webkit nil))
;;              (if (or (null string) (string-empty-p string) (string-equal symbol string))
;;                  (lsp-ui-doc--hide-frame)
;;                (lsp-ui-doc--render-buffer string symbol)
;;                (if (lsp-ui-doc--inline-p)
;;                    (lsp-ui-doc--inline)
;;                  (unless (lsp-ui-doc--get-frame)
;;                    (lsp-ui-doc--set-frame (lsp-ui-doc--make-frame)))
;;                  (unless lsp-ui-doc-use-webkit
;;                    (lsp-ui-doc--resize-buffer)
;;                    (lsp-ui-doc--move-frame (lsp-ui-doc--get-frame)))
;;                  (unless (frame-visible-p (lsp-ui-doc--get-frame)))))))

(use-package dap-mode
       :defines dap-python-executable
       :functions dap-hydra/nil
       :diminish
       :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra))
       :hook ((after-init . dap-mode)
              (dap-mode . dap-ui-mode)
              (dap-session-created . (lambda (_args) (dap-hydra)))
              (dap-stopped . (lambda (_args) (dap-hydra)))
              (dap-terminated . (lambda (_args) (dap-hydra/nil)))

              (python-mode . (lambda () (require 'dap-python)))
              (ruby-mode . (lambda () (require 'dap-ruby)))
              (go-mode . (lambda () (require 'dap-go)))
              (java-mode . (lambda () (require 'dap-java)))
              ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
              (php-mode . (lambda () (require 'dap-php)))
              (elixir-mode . (lambda () (require 'dap-elixir)))
              ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
              (powershell-mode . (lambda () (require 'dap-pwsh))))
       :init
       (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls))
       (when (executable-find "python3")
         (setq dap-python-executable "python3")))

(provide 'init-lsp)
