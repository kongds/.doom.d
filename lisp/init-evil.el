(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(require 'evil)
(require 'evil-collection)
(evil-collection-init)

(define-key evil-normal-state-map "gj" 'evil-ace-jump-char-to-mode)
(evil-collection-define-key 'normal 'dired-mode-map (kbd "SPC") 'counsel-M-x)


(require 'rtags)
;;(require 'evil-anzu)
(require 'helm)

(global-anzu-mode 1)

(evil-mode 1)

(evil-set-initial-state 'term-mode 'emacs)

(evil-define-key 'normal 'global "u" 'undo-only)
(evil-ex-define-cmd "q" 'kill-this-buffer)

(eval-after-load "rtags"
  '(progn
     (evil-set-initial-state 'rtags-mode 'emacs)
     (evil-set-initial-state 'rtags-location-stack-visualize-mode 'emacs)
     (evil-set-initial-state 'docker-image-mode 'emacs)
     (evil-set-initial-state 'docker-container-mode 'emacs)))

;; (require 'evil-multiedit)
;; (evil-multiedit-default-keybinds)

;; evil-escape-mode slow down the j and k 
;; (evil-escape-mode)
;; (setq-default evil-escape-key-sequence "jk")
;; (setq-default evil-escape-delay 0.1)

;;(require 'evil-collection)
;;(setq evil-want-keybinding nil)
;;(evil-collection-init 'vterm)

(require 'vimish-fold)
(require 'evil-vimish-fold)

(evil-define-operator evil-vimish-fold/create (beg end)
  "Create a fold from the current region.
See also `evil-delete-fold'."
  (when vimish-fold-mode
    (vimish-fold beg (1- end)))) ;; end sub 1 to make no select next line
(setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
(global-evil-vimish-fold-mode 1)

(provide 'init-evil)
