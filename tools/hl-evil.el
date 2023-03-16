(defvar hl-last-string nil)
(defvar hl-timer nil)

(defun hl-use-evil ()
  (interactive)
  (let ((str (evil-find-thing t 'symbol))
        (ca (char-after)))
    ;;(message (prin1-to-string evil-ex-search-count))
    (cond
     ((equal major-mode 'vterm-mode)
      nil)
     ((not (and ca
                (or (and (>= ca ?a)  (<= ca ?z))
                    (and (>= ca ?A)  (<= ca ?Z))
                    (member ca '(?_ ?-)))))
      (setq hl-last-string nil)
      (evil-ex-delete-hl 'evil-ex-search))
     ((equal evil-state 'insert)
      (when (< evil-ex-search-count 0)
        (setq hl-last-string nil)
        (evil-ex-delete-hl 'evil-ex-search)))
     ((or (null str)
          (equal str hl-last-string)
          (and (evil-ex-hl-active-p 'evil-ex-search)
               (or (not (numberp evil-ex-search-count))
                   (> evil-ex-search-count 0)))) nil)
     (t
      (setq hl-last-string str)
      (let ((regex (format "\\_<%s\\_>"
                           (regexp-quote str))))
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
      ))))

(defun hl-timer-toggle ()
  (interactive)
  (cond
   (hl-timer
    (cancel-timer hl-timer)
    (setq hl-timer nil))
   (t
    (setq hl-timer (run-with-idle-timer 0.5 t #'hl-use-evil)))))
