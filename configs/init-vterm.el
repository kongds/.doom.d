;;; configs/init-vterm.el -*- lexical-binding: t; -*-

;; code based on https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-shell.el
;; Shell Pop: leverage `popper'
(defvar vterm-pop--frame nil)
(defvar vterm-pop--window nil)
(defvar-local +vterm--id nil )

(defun vterm-pop--shell ()
  "Run shell and return the buffer."
  (let* ((buffer-name
          (format "*doom:vterm-popup:%s*"
                  (if (bound-and-true-p persp-mode)
                      (safe-persp-name (get-current-persp))
                    "main")))
         (buffer (or (cl-loop for buf in (doom-buffers-in-mode 'vterm-mode)
                              if (equal (buffer-local-value '+vterm--id buf)
                                        buffer-name)
                              return buf)
                     (get-buffer-create buffer-name))))
    (with-current-buffer buffer
      (setq-local +vterm--id buffer-name)
      (unless (eq major-mode 'vterm-mode)
        (vterm-mode)))
    buffer))

(defvar vterm-pop-current-window-config nil)
(defun vterm-pop ()
  (interactive)
  (cond ((and (eq major-mode 'vterm-mode)
              (string-match-p (format "*doom:vterm-popup:%s*"
                                      (if (bound-and-true-p persp-mode)
                                          (safe-persp-name (get-current-persp))
                                        "main"))
                              (buffer-name)))
         (set-window-configuration vterm-pop-current-window-config))
        (t
         (setq vterm-pop-current-window-config (current-window-configuration))
         (switch-to-buffer (vterm-pop--shell))
         (evil-insert 0))))


(after! vterm
  ;; make tqdm bar look better
  (set-fontset-font t
                    '(#x2580 . #x2590)
                    (font-spec :family "苹方-简" :size 8)
                    nil 'prepend)

  (dolist (code '(#x2500 #x250C #x2510 #x253c #x2518 #x2514 #x252c #x2534))
     (set-fontset-font t
                    `(,code . ,code)
                    (font-spec :family "苹方-简" :size 8)
                    nil 'prepend))


  (set-popup-rule! "^vterm" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)
  (set-popup-rule! "^\*vterm\*" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)
  (set-popup-rule! "^\*doom:vterm.*" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))
    (define-key vterm-mode-map [remap backward-kill-word] #'vterm-send-meta-backspace)
  (define-key vterm-mode-map (kbd "<insert-state> C-w") nil)
  (define-key vterm-mode-map (kbd "<insert-state> C-c") 'vterm-send-C-c)
  (define-key vterm-mode-map (kbd "<insert-state> C-b") 'vterm-send-C-b)
  (define-key vterm-mode-map (kbd "<insert-state> C-l") 'vterm--self-insert)
  (define-key vterm-mode-map (kbd "<insert-state> C-w h") 'evil-window-left)

  (defun vterm-insert-CUDA-VISIBLE-DEVICES ()
    (interactive)
    (vterm-goto-char (vterm--get-prompt-point))
    (call-interactively #'evil-insert)
    (vterm-send-string "CUDA_VISIBLE_DEVICES="))
  (define-key vterm-mode-map (kbd "<normal-state> C") #'vterm-insert-CUDA-VISIBLE-DEVICES)

  (after! evil-escape
    (delq! 'vterm-mode evil-escape-excluded-major-modes)))
