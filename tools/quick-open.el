;;; lisp/test.el -*- lexical-binding: t; -*-
(defvar *quick-open-histroy* nil)

(defvar highlight 'highlight)

(define-derived-mode quick-open-mode fundamental-mode "quick-open-mode"
  (read-only-mode)
  (setq-local ne-highlights
              (append
               (let ((body '())
                     (rep "\/[^\/]*")
                     (c "^"))
                 (dotimes (i 9)
                   (setq c (concat c rep))
                   (push `(,c 0
                              ,(intern (format "quick-open-depth-%d-face" (1+ i))) t) body))
                 body)
               '(("\.[^\.]*$" . font-lock-function-name-face))
              `((,(car *quick-open-histroy*) 0  highlight t))))
  (setq font-lock-defaults '(ne-highlights)))

(eval (let ((faces '())
      (light-colors ["#707183" "#7388d6" "#909183" "#709870" "#907373"
                     "#6276ba" "#858580" "#80a880" "#887070"])
      (dark-colors ["grey55" "#93a8c6" "#b0b1a3" "#97b098" "#aebed8"
                    "#b0b0b3" "#90a890" "#a2b6da" "#9cb6ad"]))
  (dotimes (i 9)
    (push `(defface ,(intern (format "quick-open-depth-%d-face" (1+ i)))
             '((default (:inherit rainbow-delimiters-base-face))
               (((class color) (background light)) :foreground ,(aref light-colors i))
               (((class color) (background dark)) :foreground ,(aref dark-colors i)))
             ,(format "Nested delimiter face, depth %d." (1+ i)))
          faces)
    (push `(defvar ,(intern (format "quick-open-depth-%d-face" (1+ i)))
             ',(intern (format "quick-open-depth-%d-face" (1+ i))))
          faces))
  `(progn ,@faces)))

(set-popup-rule! "^\*ff:" :size 0.25 :vslot -4 :select t :quit t :ttl 0)
(defun quick-open (fileName)
  "quick open file use mdfind(only in osx)"
  (interactive
   (let ((*fileName* (read-string "file:" nil '*quick-open-histroy* '*quick-open-histroy*)))
     (list *fileName*)))

  (let ((buffer (concat "*ff:" fileName "*")))
    (start-process buffer buffer "mdfind" "-name" fileName)
    (pop-to-buffer-same-window buffer)
    (quick-open-mode)
    (sleep-for 0 500)
    (beginning-of-buffer)))

(defun quick-open-open-at-point ()
  (interactive)
  (let ((file-path
         (buffer-substring-no-properties
          (point-at-bol) (point-at-eol))))
    (kill-buffer (buffer-name))
    (find-file file-path)))

(map!
 "s-o" #'quick-open
 (:leader
  :desc "quick open" :n "o q" #'quick-open)
 (:map quick-open-mode-map
  :n "q" #'(lambda()
             (interactive)
             ;;(setq kill-buffer-query-functions nil)
             (kill-this-buffer))
             ;;(setq kill-buffer-query-functions t))
  :n "RET" #'quick-open-open-at-point))

(provide 'quick-open)
