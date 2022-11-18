;;; configs/init-blink-search.el -*- lexical-binding: t; -*-

(use-package! blink-search
  :config
  (setq blink-search-quick-keys '("g" "j" "k" "l" "u"
                                  "," "." ";" "/" "'"
                                  "s" "n" "i" "o" "p"
                                  "7" "8" "9" "0"
                                  "d" "b" "a" "e" "c"
                                  "f" "r" "x" "b"
                                  "1" "2" "3" "4"
                                  "[" "]"))
  (dolist (key blink-search-quick-keys)
    (define-key blink-search-mode-map (kbd (format "s-%s" key)) 'blink-search-quick-do))

  (require 'blink-search-grep-file)
  (require 'blink-search-current-buffer)
  (defun blink-search-current-buffer-preview (buffer line column)
    (blink-search-select-input-window
     (blink-search-current-buffer-do buffer line column)))

  (defun blink-search-grep-file-preview (file line column)
    (blink-search-select-input-window
     (let ((match-file (blink-search-grep-file-get-match-buffer file)))
       (blink-search-grep-file-do file line column)
       (unless match-file
         (add-to-list 'blink-search-grep-file-temp-buffers (current-buffer))
         ))))

  (defun blink-search-imenu-preview (point)
    (blink-search-select-input-window
     (switch-to-buffer blink-search-start-buffer)
     (blink-search-imenu-do point)))

  (map! :n "t" #'blink-search))
