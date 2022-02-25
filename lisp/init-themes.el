(require 'doom-themes)
(require 'solaire-mode)

;;(set-face-font 'default "Roboto Mono Light 14")
(set-face-font 'default "Roboto Mono")

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(load-theme 'tao-yin t)
;;(add-to-list 'load-path "/Users/royokong/emacs_configs/elegant-emacs")
;;(require 'elegance)
;;(require 'sanity)


;;doom modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-lsp t)
(setq doom-modeline-height 10)
(set-face-attribute 'mode-line nil :family "Roboto Mono Light" :height 140)
(set-face-attribute 'mode-line-inactive nil :family "Roboto Mono Light" :height 140)
(setq doom-modeline-project-detection 'project)

(set-face-attribute 'default nil :height 140)

;; brighten buffers (that represent real files)
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)

;; ...if you use auto-revert-mode:
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; You can do similar with the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

(require 'modeline-fan)
;; Enable flashing mode-line on errors
;;(doom-themes-visual-bell-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable custom neotree theme
;;(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
;;
;;;;; Settings (defaults)
;;(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;      doom-themes-enable-italic t  ; if nil, italics is universally disabled
;;
;;      ;; doom-one specific settings
;;      doom-one-brighter-modeline nil
;;      doom-one-brighter-comments nil)
;;
;;;; Load the theme (doom-one, doom-dark, etc.)
;;(load-theme 'doom-one t)
;;
;;;;; OPTIONAL
;;;; brighter source buffers (that represent files)
;;(add-hook 'find-file-hook #'doom-buffer-mode-maybe)
;;;; ...if you use auto-revert-mode
;;(add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
;;;; And you can brighten other buffers (unconditionally) with:
;;(add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)
;;
;;;; brighter minibuffer when active
;;(add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)
;;
;;;; Enable custom neotree theme
;;(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
;;
;;;; Enable nlinum line highlighting
;;(doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(require-package 'color-theme-sanityinc-solarized)
;;(require-package 'color-theme-sanityinc-tomorrow)
;;
;;;; If you don't customize it, this is the theme you get.
;;(setq-default custom-enabled-themes '(sanityinc-solarized-light))
;;
;;;; Ensure that themes will be applied even if they have not been customized
;;(defun reapply-themes ()
;;  "Forcibly load the themes listed in `custom-enabled-themes'."
;;  (dolist (theme custom-enabled-themes)
;;    (unless (custom-theme-p theme)
;;      (load-theme theme)))
;;  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))
;;
;;(add-hook 'after-init-hook 'reapply-themes)
;;
;;
;;;;------------------------------------------------------------------------------
;;;; Toggle between light and dark
;;;;------------------------------------------------------------------------------

(defun light ()
  "Activate a light color theme."
  (interactive)
  (load-theme 'tao-yang t))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (load-theme 'tao-yin t))

(defcustom load-theme-before-hook nil
  "Functions to run before load theme."
  :type 'hook)

(defcustom load-theme-after-hook nil
  "Functions to run after load theme."
  :type 'hook)

(defun load-theme-hook-wrapper (origin-func theme &rest args)
  "A wrapper of hooks around `load-theme'."
  (mapc #'disable-theme custom-enabled-themes)
  (run-hook-with-args 'load-theme-before-hook theme)
  (apply origin-func theme args)
  (run-hook-with-args 'load-theme-after-hook theme))

(advice-add 'load-theme :around #'load-theme-hook-wrapper)

(provide 'init-themes)
