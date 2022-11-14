;;; get-gpu-stats.el -*- lexical-binding: t; -*-

(defun get-gpu-status ()
  "quick open file use mdfind(only in osx)"
  (interactive)
  ;;(call-process-shell-command (concat "get_paper " paper) nil 0)
  (start-process "*gpu_status*" "*gpu_status*" "python" "/Users/royokong/.doom.d/tools/get-gpu-status.py")
  (defun startsync-controller-repl-filter (proc string)
    (message string))
  (set-process-filter (get-buffer-process "*gpu_status*") 'startsync-controller-repl-filter)
  (set-process-sentinel
   (get-buffer-process "*gpu_status*")
   (lambda (p e)
     (when (= 0 (process-exit-status p))
       (let ((buf (process-buffer p)))
           (kill-buffer buf))))))


(provide 'get-gpu-status)
