;;; configs/init-vterm.el -*- lexical-binding: t; -*-

;; code based on https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-shell.el
;; Shell Pop: leverage `popper'
(defvar vterm-pop--frame nil)
(defvar vterm-pop--window nil)
(defvar-local +vterm--id nil )

(defun vterm-pop--shell (&optional arg)
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

(defun vterm-pop--hide-frame ()
  "Hide child frame and refocus in parent frame."
  (when (and (frame-live-p vterm-pop--frame)
             (frame-visible-p vterm-pop--frame))
    (make-frame-invisible vterm-pop--frame)
    (select-frame-set-input-focus (frame-parent vterm-pop--frame))
    (setq vterm-pop--frame nil)))

(defun vterm-pop-toggle ()
  "Toggle shell."
  (interactive)
  (vterm-pop--hide-frame)
  (if (window-live-p vterm-pop--window)
      (progn
        (delete-window vterm-pop--window)
        (setq vterm-pop--window nil))
    (setq vterm-pop--window
          (get-buffer-window (vterm-pop--shell)))))

(defun vterm-pop-posframe-hidehandler (_)
  "Hidehandler used by `vterm-pop-posframe-toggle'."
  (not (eq (selected-frame) vterm-pop--frame)))

(defun vterm-pop-posframe-toggle ()
  "Toggle shell in child frame."
  (interactive)
  (if (and (frame-live-p vterm-pop--frame)
           (frame-visible-p vterm-pop--frame))
      (progn
        ;; Hide child frame and refocus in parent frame
        (make-frame-invisible vterm-pop--frame)
        (select-frame-set-input-focus (frame-parent vterm-pop--frame))
        (setq vterm-pop--frame nil))
    (let* ((width  (max 100 (round (* (frame-width) 0.62))))
           (height (round (* (frame-height) 0.62)))
           (buffer (vterm-pop--shell))
           (window (get-buffer-window buffer)))
      ;; Hide window: for `popper'
      (when (window-live-p window)
        (delete-window window))

      ;; Shell pop in child frame
      (setq vterm-pop--frame
            (posframe-show
             buffer
             :poshandler #'posframe-poshandler-frame-center
             :width width
             :height height
             :min-width width
             :min-height height
             :border-width 2
             :border-color "gray"
             :override-parameters '((cursor-type . t))
             :respect-mode-line t
             :accept-focus t))

      ;; Focus in child frame
      (select-frame-set-input-focus vterm-pop--frame)
      (with-current-buffer buffer
        (evil-insert 0)))))

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
  (after! evil-escape
    (delq! 'vterm-mode evil-escape-excluded-major-modes)))
