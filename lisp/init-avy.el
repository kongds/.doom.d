(require 'ace-jump-mode)
(require 'avy)

(setq avy-background t)

(require 'tao-theme)
(tao-with-color-variables  tao-theme-yin-palette
                           (eval `(set-face-attribute 'avy-lead-face  nil :foreground ,color-14 :background ,color-3 :inverse-video nil :bold t))
                           (eval `(set-face-attribute 'avy-lead-face-0  nil :foreground ,color-14 :background ,color-5 :inverse-video nil :bold t))
                           (eval `(set-face-attribute 'avy-lead-face-1  nil :foreground ,color-14 :background ,color-7 :inverse-video nil :bold t))
                           (eval `(set-face-attribute 'avy-lead-face-2  nil :foreground ,color-14 :background ,color-9 :inverse-video nil :bold t))
                           (eval `(set-face-attribute 'avy-background-face nil :foreground ,color-6 :background ,color-2 :inverse-video nil)))

;;(require 'ace-isearch)

;;use ace-jump-char to replace avy-goto-char
(global-unset-key (kbd "C-;"))
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-line)
;;(define-key evil-normal-state-map (kbd "s") 'avy-goto-char)

(custom-set-variables
 '(ace-isearch-input-length 7)
 '(ace-isearch-jump-delay 0.25)
 '(ace-isearch-function 'avy-goto-char)
 '(ace-isearch-use-jump 'printing-char))

(define-key isearch-mode-map (kbd "C-;") 'ace-isearch-jump-during-isearch)

(provide 'init-avy)
