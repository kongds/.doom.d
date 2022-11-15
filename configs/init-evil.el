;;; configs/init-evil.el -*- lexical-binding: t; -*-

(after! evil
  ;; evil move between frame
  (defun -my-evil-move-window (&optional left)
    (condition-case nil (if left (windmove-left) (windmove-right))
      (error
       (when (> (length (frame-list)) 1)
         (remove-function after-focus-change-function #'eaf--mac-focus-change)
         (+evil/next-frame 1)
         (run-with-timer 0.1 nil
                         (lambda ()
                           (add-function :after after-focus-change-function #'eaf--mac-focus-change)))
         ))))

  (defun my-evil-move-left-window (args)
    (interactive "p")
    (-my-evil-move-window t))

  (defun my-evil-move-right-window (args)
    (interactive "p")
    (-my-evil-move-window nil))

  (evil-global-set-key 'normal (kbd "C-w h") #'my-evil-move-left-window)
  (evil-global-set-key 'normal (kbd "C-w l") #'my-evil-move-right-window)
  (evil-global-set-key 'normal (kbd "C-w C-h") #'my-evil-move-left-window)
  (evil-global-set-key 'normal (kbd "C-w C-l") #'my-evil-move-right-window)

  (global-unset-key (kbd "C-w"))
  (global-set-key (kbd "C-w h") #'my-evil-move-left-window)
  (global-set-key (kbd "C-w l") #'my-evil-move-right-window)
  (global-set-key (kbd "C-w C-h") #'my-evil-move-left-window)
  (global-set-key (kbd "C-w C-l") #'my-evil-move-right-window)

  (evil-global-set-key 'normal (kbd "q") #'kill-this-buffer)
  (evil-global-set-key 'normal (kbd "Q") #'evil-record-macro)

  ;; disable evil insert C-n C-p
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)

  (map!
   (:leader
    :desc "evil window right" :n "w l" #'my-evil-move-right-window
    :desc "evil window left" :n "w h" #'my-evil-move-left-window
    )))
