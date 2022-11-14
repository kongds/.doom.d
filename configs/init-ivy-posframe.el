;;; configs/init-ivy-posframe.el -*- lexical-binding: t; -*-

(after! ivy-posframe
  ;; remove randomly blink
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
      (setq-local truncate-lines ivy-truncate-lines))))
