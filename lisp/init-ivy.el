(require 'ivy)

;; postframe
(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
(ivy-mode 1)
(ivy-posframe-mode 1)

;; rich information
(require 'ivy-rich)
(require 'all-the-icons-ivy-rich)

(all-the-icons-ivy-rich-mode 1)
(ivy-rich-mode 1)

(define-key evil-normal-state-map (kbd "SPC") 'counsel-M-x)
(define-key evil-visual-state-map (kbd "SPC") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x d") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-recentf)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "M-?") 'counsel-ag)

(global-set-key (kbd "C-s") 'swiper-isearch)


(require 'ivy-avy)

(defvar ivy-posframe--first-show t)
(defun ivy-posframe-cleanup ()
  "Cleanup ivy's posframe."
  (setq ivy-posframe--first-show t)
  (when (posframe-workable-p)
    (posframe-hide ivy-posframe-buffer)))
(defun ivy-posframe--display (str &optional poshandler)
  "Show STR in ivy's posframe with POSHANDLER."
  (if (not (posframe-workable-p))
      (ivy-display-function-fallback str)
    (with-ivy-window
      (if (not ivy-posframe--first-show)
          (with-current-buffer ivy-posframe-buffer
            (erase-buffer)
            (insert str))
          (setq ivy-posframe--first-show nil)
          (apply #'posframe-show
                 ivy-posframe-buffer
                 :font ivy-posframe-font
                 :string str
                 :position (point)
                 :poshandler poshandler
                 :background-color (face-attribute 'ivy-posframe :background nil t)
                 :foreground-color (face-attribute 'ivy-posframe :foreground nil t)
                 :internal-border-width ivy-posframe-border-width
                 :internal-border-color (face-attribute 'ivy-posframe-border :background nil t)
                 :override-parameters ivy-posframe-parameters
                 (funcall ivy-posframe-size-function)))
      (ivy-posframe--add-prompt 'ignore)))
  (with-current-buffer ivy-posframe-buffer
    (setq-local truncate-lines ivy-truncate-lines)))

(provide 'init-ivy)
