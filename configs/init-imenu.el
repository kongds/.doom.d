;;; configs/init-imenu.el -*- lexical-binding: t; -*-

(after! imenu-list
  :config
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select nil :ttl 0)
  (defface hl-highlight
  '((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 16) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 16) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'hl-line)
  (add-hook 'imenu-list-major-mode-hook
          (lambda ()
            (hl-line-mode -1)
            (setq hl-line-overlay
                  (let ((ol (make-overlay (point) (point))))
                    (overlay-put ol 'priority -50)           ;(bug#16192)
                    (overlay-put ol 'face 'hl-highlight)
                    ol))
            (hl-line-mode 1))))
