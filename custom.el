(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(+modeline-global-mode t)
 '(+modeline-height 20)
 '(ansi-color-names-vector
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(auto-save-default t)
 '(blink-search-enable-posframe t)
 '(citar-bibliography
   '("/Users/royokong/library.bib" "/Users/royokong/mendeley.bib/library.bib"))
 '(citar-notes-paths '("/Users/royokong/.org/notes"))
 '(company-dabbrev-downcase nil t)
 '(company-dabbrev-ignore-case nil t)
 '(company-dabbrev-other-buffers nil t)
 '(company-global-modes nil)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 2)
 '(company-require-match 'never)
 '(company-show-quick-access t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-limit 10)
 '(compilation-scroll-output t)
 '(conda-anaconda-home "/opt/homebrew/Caskroom/miniforge/base/")
 '(consult-locate-args "mdfind")
 '(custom-safe-themes
   '("1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "bb5bf089d245bfed9a6d123694895c29b2bd921337150b47499f62d86d3248ca" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "88f59acbeacefb4998f45126d4d8ae8b2184f2a48753db362a349fd55321c7e1" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "f4876796ef5ee9c82b125a096a590c9891cec31320569fc6ff602ff99ed73dca" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" default))
 '(dash-docs-browser-func 'eaf-browse-url)
 '(dired-dwim-target t)
 '(display-time-format "")
 '(doom-big-font-mode t t)
 '(eaf-browser-keybinding
   '(("C--" . "zoom_out")
     ("C-=" . "zoom_in")
     ("C-0" . "zoom_reset")
     ("/" . "search_text_forward")
     ("?" . "search_text_backward")
     ("C-n" . "scroll_up")
     ("C-p" . "scroll_down")
     ("C-f" . "scroll_right")
     ("C-b" . "scroll_left")
     ("C-v" . "scroll_up_page")
     ("C-y" . "yank_text")
     ("M-e" . "atomic_edit")
     ("M-c" . "caret_toggle_browsing")
     ("M-D" . "select_text")
     ("M-s" . "open_link")
     ("M-S" . "open_link_new_buffer")
     ("M-B" . "open_link_background_buffer")
     ("C-/" . "undo_action")
     ("M-_" . "redo_action")
     ("M-w" . "copy_text")
     ("s-v" . "yank_text")
     ("i" . "atomic_edit")
     ("M-f" . "history_forward")
     ("M-b" . "history_backward")
     ("M-q" . "delete_cookie")
     ("M-Q" . "delete_all_cookies")
     ("C-t" . "toggle_password_autofill")
     ("C-d" . "save_page_password")
     ("M-a" . "toggle_adblocker")
     ("C-M-q" . "clear_history")
     ("C-M-i" . "import_chrome_history")
     ("C-M-s" . "import_safari_history")
     ("M-v" . "scroll_down_page")
     ("M-<" . "scroll_to_begin")
     ("M->" . "scroll_to_bottom")
     ("M-p" . "duplicate_page")
     ("M-t" . "new_blank_page")
     ("M-d" . "toggle_dark_mode")
     ("SPC" . "insert_or_scroll_up_page")
     ("J" . "insert_or_select_left_tab")
     ("K" . "insert_or_select_right_tab")
     ("j" . "insert_or_scroll_up")
     ("k" . "insert_or_scroll_down")
     ("h" . "insert_or_scroll_left")
     ("l" . "insert_or_scroll_right")
     ("f" . "insert_or_open_link")
     ("F" . "insert_or_open_link_new_buffer")
     ("O" . "insert_or_open_link_new_buffer_other_window")
     ("B" . "insert_or_open_link_background_buffer")
     ("c" . "insert_or_caret_at_line")
     ("u" . "insert_or_scroll_down_page")
     ("d" . "insert_or_scroll_up_page")
     ("H" . "insert_or_history_backward")
     ("L" . "insert_or_history_forward")
     ("t" . "insert_or_new_blank_page")
     ("T" . "insert_or_recover_prev_close_page")
     ("I" . "insert_or_open_downloads_setting")
     ("r" . "insert_or_refresh_page")
     ("g" . "insert_or_scroll_to_begin")
     ("q" . "insert_or_close_buffer")
     ("x" . "insert_or_close_buffer")
     ("G" . "insert_or_scroll_to_bottom")
     ("-" . "insert_or_zoom_out")
     ("=" . "insert_or_zoom_in")
     ("0" . "insert_or_zoom_reset")
     ("m" . "insert_or_save_as_bookmark")
     ("o" . "insert_or_open_browser")
     ("y" . "insert_or_download_youtube_video")
     ("Y" . "insert_or_download_youtube_audio")
     ("p" . "handle_search_backward")
     ("P" . "insert_or_duplicate_page")
     ("1" . "insert_or_save_as_pdf")
     ("2" . "insert_or_save_as_single_file")
     ("3" . "insert_or_save_as_screenshot")
     ("v" . "insert_or_view_source")
     ("e" . "insert_or_edit_url")
     ("n" . "handle_search_forward")
     ("," . "insert_or_switch_to_reader_mode")
     ("." . "insert_or_translate_text")
     (";" . "insert_or_translate_page")
     ("C-M-c" . "copy_code")
     ("C-M-l" . "copy_link")
     ("C-a" . "select_all_or_input_text")
     ("M-u" . "clear_focus")
     ("C-j" . "open_downloads_setting")
     ("M-o" . "eval_js")
     ("M-O" . "eval_js_file")
     ("<escape>" . "clear_focus")
     ("M-," . "eaf-send-down-key")
     ("M-." . "eaf-send-up-key")
     ("M-m" . "eaf-send-return-key")
     ("<f5>" . "refresh_page")
     ("<f12>" . "open_devtools")
     ("<C-return>" . "eaf-send-ctrl-return-sequence")
     ("C-<left>" . "eaf-send-ctrl-left-sequence")
     ("C-<right>" . "eaf-send-ctrl-right-sequence")
     ("C-<delete>" . "eaf-send-ctrl-delete-sequence")
     ("C-<backspace>" . "eaf-send-ctrl-backspace-sequence")))
 '(ein:jupyter-server-command "/opt/homebrew/Caskroom/miniforge/base/bin/jupyter")
 '(ein:output-area-inlined-images t)
 '(exec-path
   '("/opt/homebrew/bin" "/Users/royokong/.cargo/bin" "/opt/homebrew/Caskroom/miniforge/base/condabin" "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Library/TeX/texbin" "/Applications/VMware Fusion.app/Contents/Public" "/usr/local/aria2/bin" "/usr/local/share/dotnet" "/opt/X11/bin" "~/.dotnet/tools" "/Library/Apple/usr/bin" "/Library/Frameworks/Mono.framework/Versions/Current/Commands" "/opt/homebrew/Cellar/emacs-mac/HEAD-c479439/libexec/emacs/28.1/aarch64-apple-darwin21.4.0"))
 '(exwm-floating-border-color "#c8c8c8")
 '(fci-rule-color "#383a42")
 '(global-company-mode t)
 '(highlight-tail-colors
   ((("#e9f1e8" "#50a14f" "green")
     . 0)
    (("#e1eef3" "#0184bc" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#81A1C1"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A3BE8C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E"))
 '(lsp-bridge-completion-popup-predicates
   '(lsp-bridge-not-only-blank-before-cursor lsp-bridge-not-match-hide-characters lsp-bridge-not-match-stop-commands lsp-bridge-not-in-string lsp-bridge-not-in-comment lsp-bridge-not-follow-complete lsp-bridge-is-evil-state lsp-bridge-is-meow-state lsp-bridge-not-complete-manually lsp-bridge-not-in-org-table))
 '(lsp-enable-file-watchers t)
 '(lsp-enable-imenu nil)
 '(lsp-session-file "/Users/royokong/.emacs.d/.local/etc/lsp-session" t)
 '(objed-cursor-color "#BF616A")
 '(org-agenda-files '("~/.org/" "~/.org/roam/"))
 '(org-agenda-inhibit-startup t)
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-span 10)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-window-setup 'current-window)
 '(org-file-apps
   '((remote . emacs)
     (auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))
 '(org-startup-with-latex-preview t)
 '(package-selected-packages '(epc forge))
 '(pdf-view-midnight-colors (cons "#ECEFF4" "#2E3440"))
 '(persistent-scratch-scratch-buffer-p-function '(lambda nil (string= (buffer-name) "*doom:scratch*")))
 '(projectile-globally-ignored-directories
   '("/Users/royokong/org/.attach/" "^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$" "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$"))
 '(realgud:pdb-command-name "bash debug_remote_pdb.sh")
 '(rime-deactivate-when-exit-minibuffer nil)
 '(rtags-path
   "/Users/royokong/emacs_configs/.emacs.d.doom/.local/straight/repos/rtags/bin")
 '(rustic-ansi-faces
   ["#2E3440" "#BF616A" "#A3BE8C" "#EBCB8B" "#81A1C1" "#B48EAD" "#88C0D0" "#ECEFF4"])
 '(safe-local-variable-values
   '((cmake-tab-width . 4)
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1))
     (c-file-offsets
      (arglist-intro . +))
     (org-download-screenshot-figure-size . 400)
     (vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (eval progn
      (pp-buffer)
      (indent-buffer))
     (lsp--override-calculate-lisp-indent? . t)
     (flycheck-disabled-checkers quote
      (emacs-lisp-checkdoc))
     (eval progn
      (let
          ((dirloc-lsp-defun-regexp
            (concat
             (concat "^\\s-*(" "lsp-defun" "\\s-+\\(")
             (or
              (bound-and-true-p lisp-mode-symbol-regexp)
              "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
             "\\)")))
        (add-to-list 'imenu-generic-expression
                     (list "Functions" dirloc-lsp-defun-regexp 1)))
      (defvar lsp--override-calculate-lisp-indent? nil "Whether to override `lisp-indent-function' with\12              the updated `calculate-lisp-indent' definition from\12              Emacs 28.")
      (defun wrap-calculate-lisp-indent
          (func &optional parse-start)
        "Return appropriate indentation for current line as Lisp code.\12In usual case returns an integer: the column to indent to.\12If the value is nil, that means don't change the indentation\12because the line starts inside a string.\12\12PARSE-START may be a buffer position to start parsing from, or a\12parse state as returned by calling `parse-partial-sexp' up to the\12beginning of the current line.\12\12The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).\12This means that following lines at the same level of indentation\12should not necessarily be indented the same as this line.\12Then COLUMN is the column to indent to, and CONTAINING-SEXP-START\12is the buffer position of the start of the containing expression."
        (if
            (not lsp--override-calculate-lisp-indent?)
            (funcall func parse-start)
          (save-excursion
            (beginning-of-line)
            (let
                ((indent-point
                  (point))
                 state
                 (desired-indent nil)
                 (retry t)
                 whitespace-after-open-paren calculate-lisp-indent-last-sexp containing-sexp)
              (cond
               ((or
                 (markerp parse-start)
                 (integerp parse-start))
                (goto-char parse-start))
               ((null parse-start)
                (beginning-of-defun))
               (t
                (setq state parse-start)))
              (unless state
                (while
                    (<
                     (point)
                     indent-point)
                  (setq state
                        (parse-partial-sexp
                         (point)
                         indent-point 0))))
              (while
                  (and retry state
                       (>
                        (elt state 0)
                        0))
                (setq retry nil)
                (setq calculate-lisp-indent-last-sexp
                      (elt state 2))
                (setq containing-sexp
                      (elt state 1))
                (goto-char
                 (1+ containing-sexp))
                (if
                    (and calculate-lisp-indent-last-sexp
                         (> calculate-lisp-indent-last-sexp
                            (point)))
                    (let
                        ((peek
                          (parse-partial-sexp calculate-lisp-indent-last-sexp indent-point 0)))
                      (if
                          (setq retry
                                (car
                                 (cdr peek)))
                          (setq state peek)))))
              (if retry nil
                (goto-char
                 (1+ containing-sexp))
                (setq whitespace-after-open-paren
                      (looking-at
                       (rx whitespace)))
                (if
                    (not calculate-lisp-indent-last-sexp)
                    (setq desired-indent
                          (current-column))
                  (parse-partial-sexp
                   (point)
                   calculate-lisp-indent-last-sexp 0 t)
                  (cond
                   ((looking-at "\\s("))
                   ((>
                     (save-excursion
                       (forward-line 1)
                       (point))
                     calculate-lisp-indent-last-sexp)
                    (if
                        (or
                         (=
                          (point)
                          calculate-lisp-indent-last-sexp)
                         whitespace-after-open-paren)
                        nil
                      (progn
                        (forward-sexp 1)
                        (parse-partial-sexp
                         (point)
                         calculate-lisp-indent-last-sexp 0 t)))
                    (backward-prefix-chars))
                   (t
                    (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp
                     (point)
                     calculate-lisp-indent-last-sexp 0 t)
                    (backward-prefix-chars)))))
              (let
                  ((normal-indent
                    (current-column)))
                (cond
                 ((elt state 3)
                  nil)
                 ((and
                   (integerp lisp-indent-offset)
                   containing-sexp)
                  (goto-char containing-sexp)
                  (+
                   (current-column)
                   lisp-indent-offset))
                 (calculate-lisp-indent-last-sexp
                  (or
                   (and lisp-indent-function
                        (not retry)
                        (funcall lisp-indent-function indent-point state))
                   (and
                    (save-excursion
                      (goto-char indent-point)
                      (skip-chars-forward " \11")
                      (looking-at ":"))
                    (save-excursion
                      (goto-char calculate-lisp-indent-last-sexp)
                      (backward-prefix-chars)
                      (while
                          (not
                           (or
                            (looking-back "^[ \11]*\\|([ \11]+"
                                          (line-beginning-position))
                            (and containing-sexp
                                 (>=
                                  (1+ containing-sexp)
                                  (point)))))
                        (forward-sexp -1)
                        (backward-prefix-chars))
                      (setq calculate-lisp-indent-last-sexp
                            (point)))
                    (> calculate-lisp-indent-last-sexp
                       (save-excursion
                         (goto-char
                          (1+ containing-sexp))
                         (parse-partial-sexp
                          (point)
                          calculate-lisp-indent-last-sexp 0 t)
                         (point)))
                    (let
                        ((parse-sexp-ignore-comments t)
                         indent)
                      (goto-char calculate-lisp-indent-last-sexp)
                      (or
                       (and
                        (looking-at ":")
                        (setq indent
                              (current-column)))
                       (and
                        (<
                         (line-beginning-position)
                         (prog2
                             (backward-sexp)
                             (point)))
                        (looking-at ":")
                        (setq indent
                              (current-column))))
                      indent))
                   normal-indent))
                 (desired-indent)
                 (t normal-indent)))))))
      (when
          (< emacs-major-version 28)
        (advice-add #'calculate-lisp-indent :around #'wrap-calculate-lisp-indent)))
     (checkdoc-package-keywords-flag)
     (ffip-project-root . "/Users/royokong/")))
 '(telega-server-libs-prefix "/opt/homebrew/Cellar/tdlib/1.8.0")
 '(twittering-proxy-use t)
 '(unicode-fonts-skip-font-groups '(decorative low-quality-glyphs))
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil)
 '(vterm-shell "/opt/homebrew/bin/fish")
 '(wakatime-python-bin nil)
 '(warning-suppress-log-types
   '((org-element-cache)))
 '(warning-suppress-types '((jupyter) (jupyter) (defvaralias))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
