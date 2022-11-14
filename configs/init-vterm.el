;;; configs/init-vterm.el -*- lexical-binding: t; -*-

(after! vterm
  (set-popup-rule! "^vterm" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)
  (set-popup-rule! "^\*vterm\*" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)
  (set-popup-rule! "^\*doom:vterm.*" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))
    (define-key vterm-mode-map [remap backward-kill-word] #'vterm-send-meta-backspace)
  (define-key vterm-mode-map (kbd "<insert-state> C-w") nil)
  (define-key vterm-mode-map (kbd "<insert-state> C-c") 'vterm-send-C-c)
  (define-key vterm-mode-map (kbd "<insert-state> C-b") 'vterm-send-C-b)
  (define-key vterm-mode-map (kbd "<insert-state> C-l") 'vterm--self-insert)
  (define-key vterm-mode-map (kbd "<insert-state> C-w h") 'evil-window-left))
