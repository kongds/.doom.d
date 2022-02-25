;;add
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "M-SPC") 'set-mark-command)

;;keyboard-escape-quit
(global-unset-key (kbd "ESC ESC ESC"))

;; don not pop up the font
(global-unset-key (kbd "s-t"))

;;dictionary
(global-set-key (kbd "C-c d") 'bing-dict-brief)

;;unset ido-list-directory
(global-unset-key (kbd "C-x C-d"))

;;unset set_fill_colum
;;(global-unset-key (kbd "C-x f"))

;;unset magit-status
(global-unset-key (kbd "C-x g"))

;;compile
(global-set-key (kbd "C-,") 'compile)

;;remove mac set
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;;quick open
(global-set-key (kbd "M-o") 'quick-open)

;;macro repeat
(global-set-key (kbd "C-.") 'kmacro-end-and-call-macro)

;;mac key
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-w") 'delete-frame)
(global-set-key (kbd "s-h") 'iconify-frame)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-n") 'make-frame)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)

;;mu4e
(global-set-key (kbd "C-c m") 'mu4e)

(global-set-key (kbd "M-s r") 'replace-regexp)

(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map (kbd "C-w h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-w j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-w k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-w l") 'vil-window-right)
     (define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-w C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-w C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-w C-l") 'evil-window-right)
     (define-key evil-normal-state-map (kbd "z TAB") '(lambda ()
                                                        (interactive)
                                                        (message "reindent begin")
                                                        (save-excursion
                                                          (goto-char 0)
                                                          (while (not (eq (line-number-at-pos (point-max))
                                                                          (line-number-at-pos (point))))
                                                            (message (concat "line:"
                                                                             (prin1-to-string
                                                                              (line-number-at-pos (point))
                                                                              )))
                                                            
                                                            (goto-char (point-at-bol))
                                                            (indent-for-tab-command)
                                                            (forward-line)))
                                                        (message "reindent end")
                                                        ))
    
 

     (define-key evil-normal-state-map (kbd "SPC") 'counsel-M-x)
     (define-key evil-visual-state-map (kbd "SPC") 'counsel-M-x)
     (define-key evil-normal-state-map (kbd "J") 'ivy-switch-buffer)
     ;;(define-key evil-normal-state-map (kbd "J") 'helm-buffers-list)

     ;;(define-key evil-normal-state-map (kbd "K") 'royokong-dash-at-point)

     (define-key evil-normal-state-map (kbd "M-.") nil)
     (define-key evil-motion-state-map (kbd "C-z") nil)

     (define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
     (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
     (define-key evil-ex-completion-map (kbd "C-k") 'kill-line)


     )
  )

(global-set-key (kbd "C-x C-c") nil)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(require 'helpful)
;;(global-set-key (kbd "C-h f") #'helpful-callable)
;;(global-set-key (kbd "C-h v") #'helpful-variable)

(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(eval-after-load "ibuffer"
  (global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers) 
)


(global-set-key (kbd "s-e") 'eshell)

(provide 'init-key)

