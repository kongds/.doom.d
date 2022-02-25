(require 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;;(require 'undo-propose)
;;(global-undo-tree-mode -1)
;;(global-set-key (kbd "C-x C-u") 'undo-propose)
;;(global-set-key (kbd "C-x u") 'undo-propose)
;;(setq undo-propose-pop-to-buffer t)


(which-function-mode 1)
;;(global-set-key (kbd "M-s s") 'anzu-query-replace)
;;(global-set-key (kbd "M-s r") 'anzu-query-replace-regexp)

;; cursor color
;;(beacon-mode -1)
;;(beacon-mode -1)
(setq-default display-line-numbers 'relative)


(add-hook 'imenu-list-major-mode-hook #'(lambda()
                                          (setq display-line-numbers nil)))

(require 'imenu-list)
(define-key imenu-list-major-mode-map (kbd "j") #'next-line)
(define-key imenu-list-major-mode-map (kbd "k") #'previous-line)
(setq-default imenu-list-size 0.1)
(global-set-key (kbd "C-c u") #'imenu-list-smart-toggle)

;;---------------
;;enable wakatime
;;---------------
(require 'wakatime-mode)
(setq wakatime-api-key "04c51493-fb46-4603-b923-0a42a9822e69")
(setq wakatime-cli-path "/usr/local/bin/wakatime")

(global-wakatime-mode)


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
            (hl-line-mode 1)))
(global-hl-line-mode t) 


(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.5)
(face-spec-set 'highlight-symbol-face
               (get 'hl-highlight 'face-defface-spec ))
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))

;; pyim
;;(add-to-list 'load-path "~/input/liberime")
(require 'pyim)
(require 'posframe)
(require 'liberime)

(global-set-key (kbd "C-\\") 'toggle-input-method)

(setq default-input-method "pyim")
(setq pyim-page-tooltip 'posframe)
(setq pyim-page-length 9)

(liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport" (file-truename "/Users/royokong/Library/Rime"))
(liberime-try-select-schema "luna_pinyin_simp")
(setq pyim-default-scheme 'rime-quanpin)
(provide 'init-editing-utils)

