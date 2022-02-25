(setq fans-kit-height 10)
(setq fans-kit-width 79)
(setq fans-list (make-list 80 0))
(setq fans-image nil)
(setq fans-count 0)

(setq fans-timer
      (run-with-timer 1 1 (lambda ()
                            (let ((fan
                                   (string-to-number
                                    (nth 1 (split-string (shell-command-to-string
                                                         "/usr/local/lib/ruby/gems/2.7.0/bin/istats fan --value-only") "\n")))))
                              (push fan fans-list))
                            (if (> (length fans-list) 80)
                                (setq fansList (cdr fans-list)))

                            (if (> fans-count 1)
                                (setq  fans-image (concat
                                                   "   "
                                                   (number-to-string (car fans-list))
                                                   "RPM "
                                                   (propertize " " 'display `(image :type xpm :data
                                                                                    ,(concat
                                                                                      (format "/* XPM */\nstatic char * sparkline_xpm[] = { \"%d %d 2 1\", \"@ c %s\", \". c none\",\n\"" (1+ fans-kit-width) (1+ fans-kit-height) (face-foreground 'default))
                                                                                      (cl-loop  for i from 0 to fans-kit-height
                                                                                                initially (setq fig "")
                                                                                                do
                                                                                                (setq fig
                                                                                                      (concat
                                                                                                       (cl-loop for j from 0 to fans-kit-width
                                                                                                                initially (setq l '())
                                                                                                                do (let* ((n (nth j fans-list))
                                                                                                                          (h (* fans-kit-height (/ (- n 2000) 4000.0))))
                                                                                                                     (if (or (eq i 0) (eq i fans-kit-height))
                                                                                                                         (setq l (append l (list (if (> (% j 2) 0) 64 46))))
                                                                                                                       (if (or (eq j 0) (eq j fans-kit-width))
                                                                                                                           (setq l (append l (list (if (> (% i 2) 0) 64 46))))
                                                                                                                         (setq l (append l (list (if (> h i) 64 46)))))))
                                                                                                                finally return (concat l))
                                                                                                       (if (> (length fig) 0) "\",\n\""  "" "")
                                                                                                       fig))
                                                                                                finally return fig)
                                                                                      "\"};") :ascent 100 :fans-kit-height 11 :fans-kit-width 80))
                                                   "  ")))
                            (setq fans-count (% (+ 1 fans-count) 3))

                            )))
;;(cancel-timer fans-timer)

(doom-modeline-def-segment fans
  "The bar regulates the height of the mode-line in GUI."
  (if (doom-modeline--active)
      fans-image
  ""))


(doom-modeline-def-modeline 'main
  '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info fans)
  '( objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))

(provide 'modeline-fan)
