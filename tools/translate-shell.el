;;; translate-shell.el --- an unofficial Emacs front-end for translate-shell (a command-line translator powered by Google Translate)

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/translate-shell.el
;; Created: Sat Jun 20 13:35:27 CST 2015
;; Version: 0.0.1
;; Keywords: google, translate

;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `translate-shell.el' is an unofficial Emacs front-end for translate-shell
;; <https://github.com/soimort/translate-shell>. translate-shell is a
;; command-line translator powered by Google Translate.
;;
;;
;; Installations
;; =============
;;
;; To install manually, make sure this file is saved in a directory in your
;; `load-path', and add the line:
;;
;;   (require 'translate-shell)
;;
;; to your Emacs initialization file.
;;
;;
;; Usage
;; =====
;;
;; You can call `translate-shell-brief' and `translate-shell' to get the
;; definition your query word. The result will display in the echo area and
;; the "*Translate Shell*" buffer. You might want to bind them to some keys.
;;
;;
;; Cache
;; =====
;;
;; The result returned from `trans' (the translate-shell command-line program)
;; are stored in the `translate-shell-brief-cache' and `translate-shell-cache'
;; variable.
;;
;; If you want to use cache even after restarting Emacs, you need tt save these
;; two variable yourself. An easy solution is using `desktop.el', see below.
;;
;;
;; Customization
;; =============
;;
;; Below is my customization around this package:
;;
;;   (use-package desktop                    ; Save buffers, windows and frames
;;     :init (desktop-save-mode)
;;     :config
;;     (add-to-list 'desktop-globals-to-save 'translate-shell-cache)
;;     (add-to-list 'desktop-globals-to-save 'translate-shell-brief-cache))
;;
;;   (use-package translate-shell
;;     :load-path "~/wip/translate-shell.el"
;;     :bind (("C-c s"   . translate-shell-brief)
;;            ("C-c S"   . translate-shell))
;;     :config
;;     ;; <https://translate.google.com> is blocked in China for no apparent
;;     ;; reason. No one ever asked my option.
;;     (setq translate-shell-command "proxychains4 -q trans -t en %s"
;;           translate-shell-brief-command "proxychains4 -q trans -brief -t zh %s"))
;;
;;
;; TODO
;; ====
;;
;; - [ ] Add more functions to the result buffer, then if needs, add a header line to indicate usage

;;; Code:
(define-derived-mode translate-shell-mode fundamental-mode "translate"
  )

(after! evil-collection
  (evil-collection-define-key 'normal 'translate-shell-mode-map
    "q" (lambda ()
          (interactive)
          (kill-buffer))
    ))

(defgroup translate-shell nil
  "An unofficial Emacs front-end for translate-shell."
  :group 'tools
  :prefix "translate-shell-"
  :link '(emacs-commentary-link :tag "commentary" "translate-shell.el")
  :link '(emacs-library-link :tag "lisp file" "translate-shell.el")
  :link '(url-link :tag "Github" "https://github.com/xuchunyang/translate-shell.el"))

(defcustom translate-shell-command "trans -t zh %s"
  "The translate-shell command for the `translate-shell' command."
  :type 'string)

(defcustom translate-shell-brief-command "trans -brief -t zh %s"
  "The translate-shell command for the `translate-shell-brief' command."
  :type 'string)

(defvar translate-shell-history nil
  "History list for `translate-shell' and `translate-shell-brief'.")

(defvar translate-shell-cache nil
  "Cache alist for `translate-shell'.")
(defvar translate-shell-brief-cache nil
  "Cache alist for `translate-shell-brief'.")
(defvar translate-shell-rewrite-cache nil
  "Cache alist for `translate-rewrite'.")

(defvar trans-results nil)

(defun translate-shell--read-string ()
  "A `read-string' wrapper for translate-shell."
  (let* ((default (if (use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (let ((word (thing-at-point 'word)))
                      (when word (substring-no-properties word)))))
         (prompt (if (stringp default)
                     (format "Google Translate (default \"%s\"): " default)
                   "Google Translate: ")))
    (deactivate-mark)
    (if (stringp default)
        default
      (read-string prompt nil 'translate-shell-history default))))

;;;###autoload
(defun translate-shell-brief (word)
  "Show the explanation of WORD in the echo area."
  (interactive
   (list (translate-shell--read-string)))
  (let ((word-sym (intern word)))
    (if (assq word-sym translate-shell-brief-cache)
        (message (assoc-default word-sym translate-shell-brief-cache))
      (let* ((output
              (shell-command-to-string
               (format translate-shell-brief-command (shell-quote-argument word))))
             (result (substring output
                                0 (string-match "\n" output))))
        (message result)
        (add-to-list 'translate-shell-brief-cache (cons word-sym result))))))

(defun translate-shell-show (content)
  (if (get-buffer  "*Translate Shell*")
       (kill-buffer (get-buffer "*Translate Shell*")))
  (with-current-buffer (get-buffer-create "*Translate Shell*")
      (dotimes  (i 40) (insert "\n"))
      (insert content)
      (translate-shell-mode)
      (toggle-truncate-lines -1)
      (center-paragraph)

      (text-scale-increase 3)
      (display-buffer (current-buffer))
      (set-window-point (get-buffer-window (current-buffer)) (point-max))
      (recenter)

      ;; Set up `imenu'
      (setq imenu-generic-expression
            `(("Sections"
               ,(rx (and line-start (or "adjective" "adverb" "noun" "verb" "Synonyms" "Examples" "See also") line-end))
               0)))))

(set-popup-rule! "^\*Translate " :side 'left :size 0.5 :vslot -4 :select t :quit t :ttl 0)

;;;###autoload
(defun translate-shell (word)
  "Show the explanation of WORD in buffer."
  (interactive
   (list (translate-shell--read-string)))
  (let* ((word (replace-regexp-in-string "\n" " " word))
         (word (replace-regexp-in-string ".cite{.*}" "" word))
         (word (replace-regexp-in-string "~.citeauthor{.*}" "" word))
         (word (replace-regexp-in-string ".citeauthor{.*}" "" word))
         (word-sym (intern word))
         result)
    (setq trans-results "")
    (if (assq word-sym translate-shell-cache)
        (setq result (assoc-default word-sym translate-shell-cache))
      (start-process "*trans*" "*trans*"
                     "trans" "-t" "zh"
                     word)
      (defun startsync-controller-repl-filter (proc string)
        (setq trans-results (concat trans-results string)))
      (set-process-filter (get-buffer-process "*trans*") 'startsync-controller-repl-filter)
      (set-process-sentinel
       (get-buffer-process "*trans*")
       (lambda (p e)
         (when (= 0 (process-exit-status p))
           (let ((buf (process-buffer p)))
             (kill-buffer buf)
             (translate-shell-show trans-results))))))))


;;;###autoload
(defun translate-rewrite (word)
  "Show the explanation of WORD in buffer."
  (interactive
   (list (translate-shell--read-string)))
  (let* ((word (replace-regexp-in-string "\n" " " word))
         (word (replace-regexp-in-string "~.cite{.*}" "" word))
         (word (replace-regexp-in-string ".cite{.*}" "" word))
         (word (replace-regexp-in-string "~.citeauthor{.*}" "" word))
         (word (replace-regexp-in-string ".citeauthor{.*}" "" word))
         (word-sym (intern word)))
    (setq trans-results "")
    (if (assq word-sym translate-shell-rewrite-cache)
        (setq trans-results (assoc-default word-sym translate-shell-rewrite-cache))
      (start-process "*trans*" "*trans*"
                     "bash" "/Users/royokong/.doom.d/trans_rewrite.sh"
                     word)
      (defun startsync-controller-repl-filter (proc string)
        (setq trans-results (concat trans-results string)))
      (set-process-filter (get-buffer-process "*trans*") 'startsync-controller-repl-filter)
      (set-process-sentinel
       (get-buffer-process "*trans*")
       (lambda (p e)
         (when (= 0 (process-exit-status p))
           (let ((buf (process-buffer p)))
             (kill-buffer buf)
             (translate-shell-show trans-results))))))))

(provide 'translate-shell)
