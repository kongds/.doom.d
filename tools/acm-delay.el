;;; acm-delay.el -*- lexical-binding: t; -*-

(defun r-acm-update ()
  ;; Adjust `gc-cons-threshold' to maximize temporary,
  ;; make sure Emacs not do GC when filter/sort candidates.
  (let* ((gc-cons-threshold most-positive-fixnum)
         (keyword (acm-get-input-prefix))
         (candidates (acm-update-candidates))
         (bounds (bounds-of-thing-at-point 'symbol)))

    ;;(message (prin1-to-string candidates))
    ;;(message (prin1-to-string keyword))
    (cond
     ;; Hide completion menu if user type first candidate completely.
     ((and (equal (length candidates) 1)
           (string-equal keyword (plist-get (nth 0 candidates) :label))
           ;; Volar always send back single emmet candidate, we need filter this condition.
           (not (string-equal "Emmet Abbreviation" (plist-get (nth 0 candidates) :annotation))))
      (acm-hide))
     ((> (length candidates) 0)
      (let* ((menu-old-cache (cons acm-menu-max-length-cache acm-menu-number-cache)))
        ;; Enable acm-mode to inject mode keys.
        (acm-mode 1)

        ;; Use `pre-command-hook' to hide completion menu when command match `acm-continue-commands'.
        (add-hook 'pre-command-hook #'acm--pre-command nil 'local)

        ;; Init candidates, menu index and offset.
        (setq-local acm-candidates candidates)
        (setq-local acm-menu-candidates
                    (cl-subseq acm-candidates
                               0 (min (length acm-candidates)
                                      acm-menu-length)))
        (setq-local acm-menu-index (if (zerop (length acm-menu-candidates)) -1 0))
        (setq-local acm-menu-offset 0)

        ;; Init colors.
        (acm-init-colors)

        ;; Record menu popup position and buffer.
        (setq acm-frame-popup-point (or (car bounds) (point)))

        ;; `posn-at-point' will failed in CI, add checker make sure CI can pass.
        ;; CI don't need popup completion menu.
        (when (posn-at-point acm-frame-popup-point)
          (setq acm-frame-popup-position (acm-frame-get-popup-position))

          ;; We need delete frame first when user switch to different frame.
          (when (and (frame-live-p acm-frame)
                     (not (eq (frame-parent acm-frame) (selected-frame))))
            (acm-delete-frames))

          ;; Create menu frame if it not exists.
          (acm-create-frame-if-not-exist acm-frame acm-buffer "acm frame")

          ;; Render menu.
          (acm-menu-render menu-old-cache))
        ))
     (t
      (acm-hide)))))


(defun r-acm-doc-show ()
  (when acm-enable-doc
    (let* ((candidate (acm-menu-current-candidate))
           (backend (plist-get candidate :backend))
           (candidate-doc
            (pcase backend
              ("lsp" (acm-backend-lsp-candidate-doc candidate))
              ("elisp" (acm-backend-elisp-candidate-doc candidate))
              ("yas" (acm-backend-yas-candidate-doc candidate))
              ("tempel" (acm-backend-tempel-candidate-doc candidate))
              (_ ""))))
      (when (and candidate-doc
                 (not (string-equal candidate-doc "")))
        ;; Create doc frame if it not exist.
        (acm-create-frame-if-not-exist acm-doc-frame acm-doc-buffer "acm doc frame")

        ;; Insert documentation and turn on wrap line.
        (with-current-buffer (get-buffer-create acm-doc-buffer)
          (erase-buffer)
          (insert candidate-doc)
          (visual-line-mode 1))

        ;; Adjust doc frame position and size.
        (acm-doc-frame-adjust)
        ))))

(defvar acm-update-timer nil)
(defvar acm-doc-timer nil)
(defvar acm-delay 0.3)

(defun acm-update ()
  (if acm-update-timer
      (cancel-timer acm-update-timer))
  (setq acm-update-timer (run-with-idle-timer acm-delay nil #'r-acm-update)))

(defun acm-doc-show ()
  (if acm-doc-timer
      (cancel-timer acm-doc-timer))
  (setq acm-doc-timer (run-with-idle-timer acm-delay nil #'r-acm-doc-show)))

(provide 'acm-delay)
