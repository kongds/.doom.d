(require 'term)
(require 'multi-term)

(defun term-send-ctrl-r () (interactive) (term-send-raw-string "\C-r"))
(defun term-send-ctrl-p () (interactive) (term-send-raw-string "\C-p"))
(defun term-send-ctrl-n () (interactive) (term-send-raw-string "\C-n"))

(setq term-bind-key-alist (cons '("C-r" . term-send-ctrl-r) term-bind-key-alist))
(setq term-bind-key-alist (cons '("C-p" . term-send-ctrl-p) term-bind-key-alist))
(setq term-bind-key-alist (cons '("C-n" . term-send-ctrl-n) term-bind-key-alist))
(setq term-bind-key-alist (cons '("s-v" . term-paste) term-bind-key-alist))
(add-hook 'term-mode-hook (lambda ()
                            (setq display-line-numbers nil)))
(add-hook 'eshell-mode-hook (lambda ()
                            (setq display-line-numbers nil)))
(add-hook 'lsp-ui-doc-frame-mode-hook (lambda ()
                            (setq display-line-numbers nil)))
(add-hook 'lsp-ui-imenu-mode-hook (lambda ()
                            (setq display-line-numbers nil)))

;; vterm
(require 'vterm)
(add-hook 'vterm-mode-hook (lambda ()
                             (setq display-line-numbers nil)
                             (evil-insert 1)
                             (setq-local global-hl-line-mode nil)
                             ))

(define-key vterm-mode-map (kbd "C-x C-e")  #'(lambda ()
                                                 (interactive)
                                                 (vterm-send-key "x" nil nil t)
                                                 (vterm-send-key "e" nil nil t)
                                                 ))
;; ;; fix vterm point issue
;; (defun evil-collection-vterm-move-to-point ()
;;   (let ((p (point)))
;;     (vterm-reset-cursor-point)
;;     (while (< p (point))
;;       (vterm-send-left)
;;       (forward-char -1))
;;     (while (> p (point))
;;       (vterm-send-right)
;;       (forward-char 1))))
;; 
;; (defun evil-collection-vterm-append-line (count &optional vcount)
;;   "Switch to Insert state at the end of the current line.
;; The insertion will be repeated COUNT times.  If VCOUNT is non nil
;; it should be number > 0. The insertion will be repeated in the
;; next VCOUNT - 1 lines below the current one."
;;   (interactive "p")
;;   (evil-append-line count vcount)
;;   (evil-collection-vterm-move-to-point))
;; 
;; (defun evil-collection-vterm-insert (count &optional vcount skip-empty-lines)
;;   (interactive
;;    (list (prefix-numeric-value current-prefix-arg)
;;          (and (evil-visual-state-p)
;;               (memq (evil-visual-type) '(line block))
;;               (save-excursion
;;                 (let ((m (mark)))
;;                   ;; go to upper-left corner temporarily so
;;                   ;; `count-lines' yields accurate results
;;                   (evil-visual-rotate 'upper-left)
;;                   (prog1 (count-lines evil-visual-beginning evil-visual-end)
;;                     (set-mark m)))))
;;          (evil-visual-state-p)))
;;   (evil-insert count vcount skip-empty-lines)
;;   (evil-collection-vterm-move-to-point))
;; 
;; (evil-collection-define-key 'normal 'vterm-mode-map
;;   "i" 'evil-collection-vterm-insert
;;   "A" 'evil-collection-vterm-append-line)


(add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))
(global-set-key (kbd "s-i") #'(lambda ()
                                (interactive)
                                  (if (get-buffer "vterm")
                                      (switch-to-buffer "vterm")
                                    (vterm))))
;; evil binding
(defun vterm-evil-insert ()
  (interactive)
  (vterm-goto-char (point))
  (call-interactively #'evil-insert))

(defun vterm-evil-append ()
  (interactive)
  (vterm-goto-char (point))
  (call-interactively #'evil-append))

(defun vterm-evil-delete ()
  "Provide similar behavior as `evil-delete'."
  (interactive)
  (let ((inhibit-read-only t)
        )
    (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
      (call-interactively 'evil-delete))))

(defun vterm-evil-change ()
  "Provide similar behavior as `evil-change'."
  (interactive)
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
      (call-interactively 'evil-change))))

(defun my-vterm-hook()
  (evil-local-mode 1)
  (evil-define-key 'normal 'local "a" 'vterm-evil-append)
  (evil-define-key 'normal 'local "d" 'vterm-evil-delete)
  (evil-define-key 'normal 'local "i" 'vterm-evil-insert)
  (evil-define-key 'normal 'local "c" 'vterm-evil-change))

(add-hook 'vterm-mode-hook 'my-vterm-hook)

(provide 'init-term)
