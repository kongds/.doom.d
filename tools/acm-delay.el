;;; acm-delay.el -*- lexical-binding: t; -*-

(fset 'r-acm-update (symbol-function 'acm-update))
(fset 'r-acm-doc-try-show (symbol-function 'acm-doc-try-show))

(setq acm-enable-tabnine nil
      acm-enable-yas nil)

(defvar acm-update-timer nil)
(defvar acm-doc-timer nil)
(defvar acm-delay 0.3)

(defun acm-update ()
  (if acm-update-timer
      (cancel-timer acm-update-timer))
  (setq acm-update-timer (run-with-idle-timer acm-delay nil #'r-acm-update)))

(defun acm-doc-try-show ()
  (if acm-doc-timer
      (cancel-timer acm-doc-timer))
  (setq acm-doc-timer (run-with-idle-timer acm-delay nil #'r-acm-doc-try-show)))

(provide 'acm-delay)
