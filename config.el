;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "royokong"
      user-mail-address "j614023177@icloud.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)
(setq doom-font (font-spec :family "Roboto Mono" :size 14))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
(after! company
  (setq company-idle-delay 0)
  (setq company-show-numbers t))

;;(after! evil-snipe (evil-snipe-mode -1))
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(setq avy-all-windows t)
(setq avy-background t)


;;(use-package! tao-theme
;;  :config
;;  (load-theme 'tao-yin t)
;;  (set-face-font 'default "Roboto Mono 13")
;;  (tao-with-color-variables  tao-theme-yin-palette
;;                             (eval `(set-face-attribute 'avy-lead-face  nil :foreground ,color-14 :background ,color-3 :inverse-video nil :bold t))
;;                             (eval `(set-face-attribute 'avy-lead-face-0  nil :foreground ,color-14 :background ,color-5 :inverse-video nil :bold t))
;;                             (eval `(set-face-attribute 'avy-lead-face-1  nil :foreground ,color-14 :background ,color-7 :inverse-video nil :bold t))
;;                             (eval `(set-face-attribute 'avy-lead-face-2  nil :foreground ,color-14 :background ,color-9 :inverse-video nil :bold t))
;;                             (eval `(set-face-attribute 'avy-background-face nil :foreground ,color-6 :background ,color-2 :inverse-video nil))))

;;(use-package! window-numbering
;;  :config
;;  (window-numbering-mode))

;;(add-load-path! "~/emacs_configs/.emacs.d/elpa/org-protocol-capture-html")
;;(use-package! org-protocol-capture-html
;;  :config
;;  (setq org-capture-templates
;;        `(("w" "Web site" entry (file ,(concat org-directory "/web.org"))
;;           "* %a :website:\n\n%U %?\n\n%:initial")
;;          ("t" "Task" entry (file+headline org-default-notes-file "Tasks")
;;           "* TODO %?\n  %u\n  %a")
;;          ("m" "Memory" entry (file+headline org-default-notes-file "Memorys")
;;           "* %?\nEntered on %U\n  %i\n  %a"))))


(use-package! wakatime-mode
  :config
  (setq wakatime-cli-path "/usr/local/bin/wakatime")
  (global-wakatime-mode))

;;(use-package! posframe)

;;(use-package! pyim
;;  :config
;;  (setq default-input-method "pyim")
;;  (setq pyim-page-tooltip 'posframe)
;;  (setq pyim-default-scheme 'rime-quanpin)
;;  (setq pyim-page-length 9))

;;(use-package! liberime)
;;(after! liberime
  ;;(liberime-build)
  ;;(liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport" (file-truename "/Users/royokong/Library/Rime"))
  ;;(liberime-try-select-schema "luna_pinyin_simp"))


;;(use-package! immortal-scratch
;;  :config
;;  (immortal-scratch-mode))
;;(use-package! persistent-scratch
;;  :config
;;  (persistent-scratch-setup-default)
;;  :hook
;;  (after-init-hook . (lambda() (with-current-buffer "*scratch*"
;;                                       (emacs-lock-mode 'kill)))))

(after! doom-modeline
  :config
  (setq doom-modeline-lsp t)
  (setq doom-modeline-height 10)
  (set-face-attribute 'mode-line nil :family "Roboto Mono Light" :height 120)
  (set-face-attribute 'mode-line-inactive nil :family "Roboto Mono Light" :height 120)
  (setq doom-modeline-project-detection 'project)
  ;;(load! "modeline-fan")
  )

;;(use-package! lsp-pyright
;;  :hook
;;  (python-mode . (lambda ()
;;                   (require 'lsp-pyright)
;;                   (lsp)))
;;  :config
;;  (when (executable-find "python3")
;;    (setq lsp-pyright-python-executable-cmd "python3")))

;;(add-load-path! "/Users/royokong/emacs_configs/.emacs.d/elpa/ein-0.16.0")
;;(use-package! ein)
;;(use-package! ein-notebook)

;; set ttl to nil means vterm will not been killed
(after! vterm
  (set-popup-rule! "^vterm" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)
  (set-popup-rule! "^\*vterm\*" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)
  (set-popup-rule! "^\*doom:vterm.*" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))
    (define-key vterm-mode-map [remap backward-kill-word] #'vterm-send-meta-backspace)
  (define-key vterm-mode-map (kbd "<insert-state> C-w") nil)
  (define-key vterm-mode-map (kbd "<insert-state> C-c") 'vterm-send-C-c)
  (define-key vterm-mode-map (kbd "<insert-state> C-l") 'vterm--self-insert)
  (define-key vterm-mode-map (kbd "<insert-state> C-w h") 'evil-window-left))

(set-popup-rule! "^\*doom:scra.*" :side 'right :size 0.5 :vslot -4 :select t :quit nil :ttl nil)

(use-package! imenu-list
  :config
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select nil :ttl 0)
  (defface hl-highlight
  '((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 16) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 16) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'hl-line)
  (add-hook 'imenu-list-major-mode-hook
          (lambda ()
            (hl-line-mode -1)
            (setq hl-line-overlay
                  (let ((ol (make-overlay (point) (point))))
                    (overlay-put ol 'priority -50)           ;(bug#16192)
                    (overlay-put ol 'face 'hl-highlight)
                    ol))
            (hl-line-mode 1))))

(use-package! treemacs
  :config
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select nil :ttl 0)
  (defface hl-highlight
  '((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 16) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 16) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'hl-line)
  (add-hook 'treemacs-mode-hook
          (lambda ()
            (hl-line-mode -1)
            (setq hl-line-overlay
                  (let ((ol (make-overlay (point) (point))))
                    (overlay-put ol 'priority -50)           ;(bug#16192)
                    (overlay-put ol 'face 'hl-highlight)
                    ol))
            (hl-line-mode 1))))

(after! lsp-mode
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\|org\\|ipynb\\)$" lsp-file-watch-ignored)
  ;;(setq +lsp-company-backends '(company-capf :with company-dabbrev-code :separate))
  ;;(setq +lsp-company-backends '(company-capf company-yasnippet company-tabnine-capf :separate))
  ;;(setq +lsp-company-backends '(company-capf company-yasnippet :separate))
  ;;(setq lsp-session-file "/dev/null")
  ;;(defun lsp-completion-mode-setup ()
    ;;(setq-local company-backends '((company-capf :with company-dabbrev-code :separate))))
  ;;(add-hook 'lsp-completion-mode-hook #'lsp-completion-mode-setup)
)


(after! evil
  ;; evil move between frame
  (evil-global-set-key 'normal (kbd "C-w h") #'(lambda ()
                                                 (interactive)
                                                 (condition-case nil
                                                     (windmove-left)
                                                   (error (if (> (length (frame-list)) 1)
                                                              (+evil/next-frame 1))))))
  (evil-global-set-key 'normal (kbd "C-w l") #'(lambda ()
                                                 (interactive)
                                                 (condition-case nil
                                                     (windmove-right)
                                                   (error (if (> (length (frame-list)) 1)
                                                              (+evil/next-frame 1))))))
  ;; center after jump
  (let ((after-fn (lambda (&rest _) (recenter nil))))
    (advice-add 'evil-goto-mark-line :after after-fn)))
    ;;(advice-add 'evil-ex-search-next :after after-fn)
    ;;(advice-add 'evil-ex-search-previous :after after-fn))
;;(defalias 'forward-evil-word 'forward-evil-symbol))

(after! goto-chg
    (let ((after-fn (lambda (&rest _) (recenter nil))))
    (advice-add 'goto-last-change :after after-fn)))

(after! better-jumper
    (let ((after-fn (lambda (&rest _) (recenter nil))))
    (advice-add 'better-jumper-jump-backward :after after-fn)
    (advice-add 'better-jumper-jump-forward :after after-fn)))

(after! dired
  (setq ranger-show-hidden t))

(after! modeline
  (setq! +modeline-height 20))

(after! core
  (defvar doom-scratch-buffer-created-hook nil)

  (defun doom-persist-scratch-buffer-h-with-cache ()
    "Save the current buffer to `doom-scratch-dir'."
    (member (current-buffer) doom-scratch-buffers)
    (let ((content (buffer-substring-no-properties (point-min) (point-max)))
          (point (point))
          (mode major-mode)
          (scratch (expand-file-name (concat (or doom-scratch-current-project
                                                 doom-scratch-default-file)
                                             ".el")
                                     doom-scratch-dir)))
      (copy-file scratch
                 (concat scratch "." (format-time-string "%H:%M:%S-%m-%d-%Y" (current-time))))
      (with-temp-file scratch
        (prin1 (list content
                     point
                     mode)
               (current-buffer)))))

  (setq doom-scratch-buffers nil)
  (defun doom-persist-scratch-buffers-h-with-cache ()
    "Save all scratch buffers to `doom-scratch-dir'."
    (setq doom-scratch-buffers
          (cl-delete-if-not #'buffer-live-p doom-scratch-buffers))
    (dolist (buffer doom-scratch-buffers)
      (with-current-buffer buffer
        (doom-persist-scratch-buffer-h-with-cache))))

  ;; show modeline in buffer
  (advice-add 'doom/open-scratch-buffer :after #'(lambda (&rest r)
                                                 (+modeline-mode 1)))

  ;;(fmakunbound 'doom-persist-scratch-buffer-h)
  ;;(defalias 'doom-persist-scratch-buffer-h #'doom-persist-scratch-buffer-h-with-cache)
  (remove-hook 'kill-emacs-hook #'doom-persist-scratch-buffers-h)
  (add-hook 'kill-emacs-hook #'doom-persist-scratch-buffers-h-with-cache)
  (add-hook 'doom-scratch-buffer-created-hook
            (lambda ()
              (setq buffer-undo-list nil)
              (remove-hook 'kill-buffer-hook #'doom-persist-scratch-buffer-h 'local)
              (add-hook 'kill-buffer-hook #'doom-persist-scratch-buffer-h-with-cache nil 'local)
              (local-set-key (kbd "C-x s") #'(lambda ()
                                               (interactive)
                                               (when (member (current-buffer) doom-scratch-buffers)
                                                 (doom-persist-scratch-buffer-h-with-cache)
                                                 (set-buffer-modified-p nil))))
              (local-set-key (kbd "s-s") (kbd "C-x s"))
              (local-set-key (kbd "C-x C-s") (kbd "C-x s")))))

(after! python
  (set-repl-handler! 'python-mode #'+python/open-ipython-repl)
  (add-hook 'python-mode-hook
            (lambda()
              (which-function-mode 1))))
              ;;(evil-local-set-key 'normal (kbd "[ [") #'python-nav-backward-block)
              ;;(evil-local-set-key 'normal (kbd "] ]") #'python-nav-forward-block))))

(after! ivy-posframe
  ;; remove randomly blink
  (defvar ivy-posframe--first-show t)
  (defun ivy-posframe-cleanup ()
    "Cleanup ivy's posframe."
    (setq ivy-posframe--first-show t)
    (when (posframe-workable-p)
      (posframe-hide ivy-posframe-buffer)))
  (defun ivy-posframe--display (str &optional poshandler)
    "Show STR in ivy's posframe with POSHANDLER."
    (if (not (posframe-workable-p))
        (ivy-display-function-fallback str)
      (with-ivy-window
        (if (not ivy-posframe--first-show)
            (with-current-buffer ivy-posframe-buffer
              (erase-buffer)
              (insert str))
            (setq ivy-posframe--first-show nil)
            (apply #'posframe-show
                   ivy-posframe-buffer
                   :font ivy-posframe-font
                   :string str
                   :position (point)
                   :poshandler poshandler
                   :background-color (face-attribute 'ivy-posframe :background nil t)
                   :foreground-color (face-attribute 'ivy-posframe :foreground nil t)
                   :internal-border-width ivy-posframe-border-width
                   :internal-border-color (face-attribute 'ivy-posframe-border :background nil t)
                   :override-parameters ivy-posframe-parameters
                   (funcall ivy-posframe-size-function)))
        (ivy-posframe--add-prompt 'ignore)))
    (with-current-buffer ivy-posframe-buffer
      (setq-local truncate-lines ivy-truncate-lines))))

;;(use-package! evil-matchit
 ;;:config
  ;;(setq evilmi-shortcut "M")
  ;;(global-evil-matchit-mode 1))

;; fast log file
(defvar fast-log-file nil)
(put 'fast-log-file 'safe-local-variable (lambda (_) t))
(map!
 (:leader
  :desc "fast log" :n "l" #'(lambda()
                              (interactive)
                              (if (+popup-windows)
                                  (progn
                                    (+popup/toggle)
                                    (balance-windows))
                                (if fast-log-file
                                    (let ((buffer (find-file-noselect fast-log-file)))
                                        (set-popup-rule! (buffer-name buffer)
                                                        :side 'right :autosave t :size 0.5
                                                        :vslot -4 :select t :quit nil :ttl nil)
                                        (pop-to-buffer-same-window buffer))
                                  (doom/open-project-scratch-buffer))))))

;; enable local and eval in .dir-local
(setq enable-local-eval t)
(setq enable-local-variables t)


(after! pdf-view
  (evil-collection-define-key 'normal 'pdf-view-mode-map
    "N" 'pdf-history-forward
    "B" 'pdf-history-backward))

;; word contains underscores
(after! python
  (add-hook 'python-mode-hook
            (lambda() (modify-syntax-entry ?_ "w"))))
(after! vterm
  (add-hook 'vterm-mode-hook
            (lambda() (modify-syntax-entry ?_ "w"))))
(add-hook 'emacs-lisp-mode-hook
        (lambda() (modify-syntax-entry ?- "w")))


(after! telega
  (setq telega-proxies
      (list
       '(:server "127.0.0.1" :port 1087 :enable t
                 :type (:@type "proxyTypeHttp")))))


(after! ein-notebook
  (ein:notebook--define-key  ein:notebook-mode-map (kbd "C-o o") ein:worksheet-insert-cell-below)
  (ein:notebook--define-key  ein:notebook-mode-map (kbd "C-o d") ein:worksheet-kill-cell)
  (ein:notebook--define-key  ein:notebook-mode-map (kbd "C-o O") ein:worksheet-insert-cell-above)
  (ein:notebook--define-key  ein:notebook-mode-map (kbd "C-j") ein:worksheet-goto-next-input)
  (ein:notebook--define-key  ein:notebook-mode-map (kbd "C-k") ein:worksheet-goto-prev-input)

  (defun ein-mode-hooks ()
    (make-local-variable 'evil-motion-state-map)
    (setq-local evil-motion-state-map (copy-tree evil-motion-state-map))
    (define-key  evil-motion-state-map (kbd "C-o") nil)
    (define-key  evil-motion-state-map (kbd "C-j") nil))
  (add-hook 'ein:notebook-mode-hook 'ein-mode-hooks))

(use-package! rime
  :config
  (setq default-input-method "rime")
  (setq rime-librime-root "~/.emacs.d/librime/dist")
  (setq rime-show-candidate 'posframe)
  (setq rime-cursor "˰")

  (defvar in-updating-cursor nil)
  (defvar rime-init nil)
  (defvar rime-enable nil)

  (global-set-key (kbd "C-\\") ;;'toggle-input-method)
                  (lambda ()
                    (interactive)
                    (setq evil-default-cursor
                          (lambda ()
                            (if (or (equal (frame-parameter nil 'cursor-color) (get 'cursor 'evil-emacs-color))
                                    (equal (frame-parameter nil 'cursor-color) (get 'cursor 'evil-normal-color)))
                                (evil-set-cursor-color (if rime-enable
                                                           (get 'cursor 'evil-emacs-color)
                                                         (get 'cursor 'evil-normal-color)))
                              (+evil-update-cursor-color-h))))
                    (unless rime-init
                      (setq rime-init t)
                      (add-hook 'input-method-activate-hook (lambda ()
                                                              (setq-local rime-enable t)
                                                              (funcall evil-default-cursor)))
                      (add-hook 'input-method-deactivate-hook (lambda ()
                                                                (setq-local rime-enable nil)
                                                                (funcall evil-default-cursor))))
                    (toggle-input-method))))

(after! realgud
  (defun realgud:file-loc-from-line-before (args)
    (mapcar #'(lambda (x)
        (if (typep x 'string)
            (replace-regexp-in-string ".mnt.jt" "/Users/royokong/nlp" x) x))
     args))
  (advice-add 'realgud:file-loc-from-line
              :filter-args 'realgud:file-loc-from-line-before)
  (advice-add 'realgud:file-column-from-string
              :filter-args 'realgud:file-loc-from-line-before)
  (advice-add 'realgud:file-line-count
              :filter-args 'realgud:file-loc-from-line-before)

  (defun realgud-send-command-before (args)
    (message (prin1-to-string args))
    (mapcar #'(lambda (x)
        (if (typep x 'string)
            (replace-regexp-in-string  ".Users.royokong.nlp" "/mnt/jt" x) x))
     args))
  (advice-add 'realgud-send-command
              :filter-args 'realgud-send-command-before)
  (defun pdb-reset ()
  "Pdb cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (pdb-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*pdb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))))))



(load! "config-org")

(load! "start-sync")

(load! "quick-open")

(load! "get-paper")

(load! "pdf-search")

(load! "run-command-with-notify")

;; (load! "corfu-company")

(load! "tabnine-capf")
(load! "copilot-company")

(load! "local")

(load! "+bindings")

(load! "+patches")

(load! "trans")
