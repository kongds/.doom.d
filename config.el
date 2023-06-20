;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "royokong"
      user-mail-address "j614023177@icloud.com")

(setenv "PATH" (concat  "/opt/homebrew/bin/" ":" (getenv "PATH")))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)
(setq doom-font (font-spec :family "Roboto Mono" :size 14)
      doom-unicode-font (font-spec :family "苹方-简"))
;;(setq doom-font (font-spec :family "Monego" :size 14))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)

;; proxy
(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
     ("http" . "127.0.0.1:1087")
     ("https" . "127.0.0.1:1087")))

;; gc set up
;; use default value, sweet point for me
(after! gcmh
  (setq gcmh-high-cons-threshold 1073741824))

;; not ignore some repos in local path
(defun my-doom-project-ignored-p-around (orig-fn project-root)
  "Around advice for `doom-project-ignored-p` to not ignore some project roots."
  (unless (string-match-p "lsp-bridge" project-root)
    (funcall orig-fn project-root)))
(advice-add 'doom-project-ignored-p :around #'my-doom-project-ignored-p-around)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;;(after! evil-snipe (evil-snipe-mode -1))
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(setq avy-all-windows t)
(setq avy-background t)

;; not use ESC as prefix key
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(set-popup-rule! "^\*doom:scra.*" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)


(add-hook 'doom-after-init-hook
          (lambda ()
            ;; vimish fold mode
            (vimish-fold-global-mode 1)))

(add-hook 'doom-first-buffer-hook
          (lambda ()
            ;; sort tab on
            (sort-tab-turn-on)

            (global-company-mode -1)

            (toggle-frame-maximized-or-fullframe)

            ;; face
            (set-fontset-font t 'han (font-spec :family "苹方-简"))))

(after! python
  (set-repl-handler! 'python-mode #'+python/open-ipython-repl)
  (add-hook 'python-mode-hook
            (lambda()
              ;;(smartparens-mode -1)
              (setq-local dash-docs-docsets '("PyTorch" "transformers" "Python 3"))
              (setq-local dash-docs-browser-func 'browse-url)
              (which-function-mode 1))))

;; enable local and eval in .dir-local
(setq enable-local-eval t)
(setq enable-local-variables t)

(after! highlight-symbol
  (set-face-attribute 'highlight-symbol-face nil :inherit 'evil-ex-lazy-highlight))

(setq +lookup-provider-url-alist (append +lookup-provider-url-alist
                                         '(("Google scholar"     "https://scholar.google.com/scholar?q=%s"))))


;; word contains underscores
(after! python
  (add-hook 'python-mode-hook
            (lambda() (modify-syntax-entry ?_ "w"))))
(after! vterm
  (add-hook 'vterm-mode-hook
            (lambda() (modify-syntax-entry ?_ "w"))))
(add-hook 'emacs-lisp-mode-hook
        (lambda() (modify-syntax-entry ?- "w")))

;; theme
(defun toggle-dark-light-theme ()
  (interactive)
  (if (equal doom-theme 'doom-nord)
      (load-theme 'ef-summer)
    (load-theme 'doom-nord)
    ;; reload to doom-nord again to
    ;; avoid blink in echo area
    (load-theme 'doom-nord))
  (when (featurep 'lsp-bridge)
      (set-face-background 'acm-frame-default-face (face-attribute 'default :background))
      (set-face-background 'acm-frame-select-face (face-attribute 'highlight :background))
      (set-face-foreground 'acm-frame-select-face (face-attribute 'highlight :foreground))
      (when lsp-bridge-is-starting
        (lsp-bridge-restart-process)
        (acm-reset-colors)
        (kill-buffer acm-buffer)
        (kill-buffer acm-doc-buffer))))

;; vertico support pyim
(after! vertico
  (require 'pyim)
  (pyim-default-scheme 'pyim-shuangpin)
  (defun my-orderless-regexp (orig-func component)
    (let ((result (funcall orig-func component)))
      (pyim-cregexp-build result)))
  (advice-add 'orderless-regexp :around #'my-orderless-regexp))


;; use ts mode
(when (and (featurep 'treesit)
         (treesit-available-p))
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
    (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))


(load! "+bindings")

(load! "+patches")

(load! "+local")

;; configs

(load! "configs/init-imenu")
;;
(load! "configs/init-treemacs")
;;
(load! "configs/init-doom-modeline")
;;
(load! "configs/init-evil")
;;
(load! "configs/init-wakatime")
;;
(load! "configs/init-vterm")
;;
(load! "configs/init-ivy-posframe")
;;
(load! "configs/init-ein-notebook")
;;
(load! "configs/init-netease-music")
;;
(load! "configs/init-rime")
;;
(load! "configs/init-telega")
;;
(load! "configs/init-realgud")
;;
(load! "configs/init-blink-search")
;;
(load! "configs/init-copilot")
;;
(load! "configs/init-color-rg")
;;
(load! "configs/init-pdf-search")
;;
(load! "configs/init-elfeed")
;;
(load! "configs/init-sort-tab")
;;
(load! "configs/init-org")
;;
(load! "configs/init-org-noter")
;;
(load! "configs/init-eaf")
;;
(load! "configs/init-corfu")
;;
(load! "configs/init-lsp-bridge")
;;
(load! "configs/init-elisp")
;;
(load! "configs/init-magit")

(doom-load-packages-incrementally '(python treesit acm lsp-bridge corfu))
(doom-load-packages-incrementally '(zmq citar elfeed jupyter evil-org))
(doom-load-packages-incrementally '(telega pdf-tools org-noter))

;; tools
(add-to-list 'load-path "/Users/royokong/.doom.d/tools/")

(use-package! start-sync :after (:any vterm python sh-script))

(use-package! work-remote-tmux :after (:any vterm python sh-script))

(use-package! quick-open :after dired)

(use-package! translate-shell :commands (translate-shell translate-rewrite))

(use-package! get-gpu-status :commands get-gpu-status)

(use-package! hl-evil :commands hl-timer-toggle)

(use-package! doctor-chatgpt :after doctor)

;;(load! "tools/start-sync")
;;
;;(load! "tools/quick-open")
;;
;;(load! "tools/trans")
;;
;;(load! "tools/work-remote-tmux")
;;
;;(load! "tools/get-gpu-status")
