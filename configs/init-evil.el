;;; configs/init-evil.el -*- lexical-binding: t; -*-

(after! evil
  ;; evil move between frame
  (defun -my-evil-move-window (&optional left)
    (condition-case nil (if left (windmove-left) (windmove-right))
      (error
       (when (and (not (eq (next-frame) (selected-frame))) (featurep 'eaf))
         (remove-function after-focus-change-function #'eaf--topmost-focus-change)
         (+evil/next-frame 1)
         (run-with-timer 0.1 nil
                         (lambda ()
                           (add-function :after after-focus-change-function #'eaf--topmost-focus-change)))
         ))))

  (defun my-evil-move-left-window (args)
    (interactive "p")
    (-my-evil-move-window t))

  (defun my-evil-move-right-window (args)
    (interactive "p")
    (-my-evil-move-window nil))

  (defun my-evil-move-next-window (args)
    (interactive "p")
    (cond
     ((eq (next-frame) (selected-frame))
       (evil-window-next nil))
     ((or (eq (length (window-list)) 1)
          (eq last-command 'my-evil-move-next-window))
      (+evil/next-frame 1))
     (t (evil-window-next nil))))

  (evil-global-set-key 'normal (kbd "C-w h") #'my-evil-move-left-window)
  (evil-global-set-key 'normal (kbd "C-w l") #'my-evil-move-right-window)
  (evil-global-set-key 'normal (kbd "C-w C-h") #'my-evil-move-left-window)
  (evil-global-set-key 'normal (kbd "C-w C-l") #'my-evil-move-right-window)
  (evil-global-set-key 'normal (kbd "K") #'evil-end-of-line)
  (evil-global-set-key 'normal (kbd "J") #'evil-first-non-blank)

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

  ;; use C-e in minibuffer
  (define-key  evil-ex-completion-map (kbd "C-e") #'move-end-of-line)

  (map!
   (:leader
    :desc "evil window right" :n "w l" #'my-evil-move-right-window
    :desc "evil window left" :n "w h" #'my-evil-move-left-window
    :desc "evil window next" :n "w w" #'my-evil-move-next-window
    :desc "evil window next" :n "l" #'my-evil-move-next-window
    )))



(after! evil-text-object
  (defmacro evil-textobj-tree-sitter-get-textobj-with-fallback (group &optional query fallback)
    (declare (debug t) (indent defun))
    (let* ((groups (if (eq (type-of group) 'string)
                       (list group)
                     group))
           (funsymbol (intern (concat "evil-textobj-tree-sitter-function--"
                                      (mapconcat 'identity groups "-"))))
           (interned-groups (mapcar #'intern groups)))
      `(evil-define-text-object ,funsymbol
         ;; rest argument is named because of compiler warning `argument _ not left unused`
         (count &rest unused)
         (if tree-sitter-mode
             (let ((range (evil-textobj-tree-sitter--range count ',interned-groups ,query)))
               (if (not (eq range nil))
                   (evil-range (car range)
                               (cdr range))
                 (evil-textobj-tree-sitter--message-not-found ',groups)))
           (message "%s" (append (list ,fallback count) unused))
           (apply (append (list ,fallback count) unused))))))


  (define-key evil-outer-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj-with-fallback ("conditional.outer" "loop.outer") nil #'evil-outer-arg))
  (define-key evil-inner-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj-with-fallback ("conditional.inner" "loop.inner") nil #'evil-inner-arg)))

  ;;(define-key evil-outer-text-objects-map "f"
  ;;            (evil-textobj-tree-sitter-get-textobj-with-fallback "function.outer" nil '+evil:defun-txtobj))
  ;;(define-key evil-inner-text-objects-map "f"
  ;;            (evil-textobj-tree-sitter-get-textobj-with-fallback "function.inner" nil '+evil:defun-txtobj))

  ;;(append (list 1 ) '(1 2 3))

  ;;(apply '(+evil:defun-txtobj 1 nil nil nil))
  ;;(define-key evil-inner-text-objects-map "f" '+evil:defun-txtobj)
