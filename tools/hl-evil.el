(defvar hl-last-string nil)
(defvar hl-last-point nil)

(defun hl-use-evil ()
  (interactive)
  (let* ((string (evil-find-thing t 'symbol))
         (cup (point))
         (patc (buffer-substring-no-properties
                cup (min (point-max) (+ cup 1)))))
    ;;(message (prin1-to-string evil-ex-search-count))
    (cond
     ((equal hl-last-point cup)
      nil)
     ((or (not (derived-mode-p 'prog-mode))
          (not (string-match-p "^[a-zA-Z-_]$" patc)))
      (when hl-last-string
        (evil-ex-delete-hl 'evil-ex-search)
        (setq hl-last-string nil))
      nil)
     ((equal evil-state 'insert)
      (when (< evil-ex-search-count 0)
        (setq hl-last-string nil)
        (evil-ex-delete-hl 'evil-ex-search)))
     ((or (null string)
          (equal string hl-last-string)
          (and (evil-ex-hl-active-p 'evil-ex-search)
               (or (not (numberp evil-ex-search-count))
                   (> evil-ex-search-count 0)))) nil)
     (t
      (setq hl-last-string string)
      (let ((regex (format "\\_<%s\\_>"
                           (regexp-quote string))))
        (setq evil-ex-search-count -1
              evil-ex-search-direction 'forward
              evil-ex-search-pattern
              (let (evil-ex-search-vim-style-regexp)
                (evil-ex-make-search-pattern regex))
              evil-ex-search-offset nil
              evil-ex-last-was-search t)
        ;; update search history unless this pattern equals the
        ;; previous pattern
        (unless (equal (car-safe evil-ex-search-history) regex)
          (push regex evil-ex-search-history))
        (evil-push-search-history regex t))
      (evil-ex-delete-hl 'evil-ex-search)
      (evil-ex-search-activate-highlight evil-ex-search-pattern)
      ))
    (setq hl-last-point cup)))

;; (remove-hook 'post-command-hook #'hl-use-evil)
(add-hook 'post-command-hook #'hl-use-evil)

(defun hl-evil-turn-off ()
  (interactive)
  (remove-hook 'post-command-hook #'hl-use-evil))

(defun hl-evil-turn-on ()
  (interactive)
  (add-hook 'post-command-hook #'hl-use-evil))

(provide 'hl-evil)
