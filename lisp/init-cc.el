(require 'cc-mode)


(defun compile-cc (isC++)
  "run the cpp at first line comment "
  (let ((compile-file (buffer-file-name))
        (out-file     (car  (split-string (buffer-name) "\\."))))
    (save-buffer)
    (compile
     (concat (if isC++ "g++ -std=c++11 " "gcc ")
             "-g "
             "-o "
             out-file
             " "
             compile-file
             ";./"
             out-file
             ))))

(define-key c++-mode-map (kbd "s-r") '(lambda () (interactive) (compile-cc t)))
(define-key c-mode-map   (kbd "s-r") '(lambda () (interactive) (compile-cc nil)))


;;;;;;;;;;;;;;;;;;;;;;
;;rtags-init
;;;;;;;;;;;;;;;;;;;;;;
(require 'rtags)
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)


;;;;;;;;;;;;;;;;;;;;;;
;;irony-init
;;;;;;;;;;;;;;;;;;;;;;
;;hooks
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook #'irony-eldoc)
;;company
;; (eval-after-load 'company
;; '(add-to-list 'company-backends 'company-irony))
(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
;; (eval-after-load 'company
;; '(add-to-list
;; 'company-backends '(company-irony-c-headers company-irony)))

;;flycheck
;; (eval-after-load 'flycheck
;; '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;; check head file to decide mode
(add-hook 'c-mode-hook (lambda ()
                         (let ((filename (nth 0  (split-string (buffer-name) "\\."))))
                           (if (file-exists-p (concat filename ".m"))
                               (objc-mode)))))


;;;;;;;;;;;;;;;;;;;;;;
;;hs-minor-moode
;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook (lambda()
                            (unless (equal major-mode 'ein:notebook-multilang-mode)
                            (hs-minor-mode))))

(provide 'init-cc)
