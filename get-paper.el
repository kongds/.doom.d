;;; lisp/test.el -*- lexical-binding: t; -*-
(defvar *get-paper-histroy* nil)

(defvar highlight 'highlight)

(defun get-paper (paper)
  "quick open file use mdfind(only in osx)"
  (interactive
   (let ((*paper* (read-string "title:" nil '*get-paper-histroy* '*quick-open-histroy*)))
     (list *paper*)))
  ;;(call-process-shell-command (concat "get_paper " paper) nil 0)
  (start-process "*get_paper*" "*get_paper*" "get_paper" paper)
  (defun startsync-controller-repl-filter (proc string)
    (message string))
  (set-process-filter (get-buffer-process "*get_paper*") 'startsync-controller-repl-filter)
  (set-process-sentinel
   (get-buffer-process "*get_paper*")
   (lambda (p e)
     (when (= 0 (process-exit-status p))
       (let ((buf (process-buffer p)))
         (when (get-buffer buf)
           (display-buffer buf t)
           (sit-for 2)
           (delete-window (get-buffer-window buf))
           (kill-buffer buf))))))
)

(map!
 (:leader
  :desc "quick open" :n "o p" #'get-paper))

(provide 'get-paper)
