;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! org-download)
(package! tao-theme)
(package! wakatime-mode)
(package! immortal-scratch)
(package! persistent-scratch)
(package! posframe)
;;(package! liberime)
;;(package! ein)
(package! window-numbering)
;;(package! lsp-pyright)
(package! ob-ipython)
(package! ob-async)
(package! imenu-list)
;;(package! auto-complete)
;;(package! skewer-mode)
;;(package! osx-dictionary)

(package! epc)
(package! ctable)
(package! s)

(package! websocket)
;; high cpu usage
;;(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! evil-matchit)

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

(package! symbol-overlay)
(package! company-tabnine)

(package! org-ref)
(package! helm-bibtex)

;; When using org-roam via the `+roam` flag
(unpin! org-roam)

;; When using bibtex-completion via the `biblio` module
(unpin! bibtex)

(package! corfu :recipe (:host github :repo "minad/corfu" :files ("*.el" "extensions/*.el")))
(package! cape)
(package! corfu-doc)
(package! kind-icon)

(package! org-modren :recipe (:host github :repo "minad/org-modern" :files ("*.el" "out")))

(package! pyim-basedict)

(package! rime)

(package! gscholar-bibtex)

(package! org-protocol-capture-html)

(package! elisp-format)

(package! org-fragtog)

(package! osx-dictionary)

(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! go-translate)

(package! vterm-toggle)

(package! netease-cloud-music :recipe (:host github :repo "SpringHan/netease-cloud-music.el" :files ("*.el")))

(package! eaf :recipe (:host github :repo "emacs-eaf/emacs-application-framework" :files ("*")))

(package! keytar)

;(package! lsp-grammarly :recipe (:host github :repo "emacs-grammarly/lsp-grammarly" :files ("*")) :pin "02926aa")
(package! lsp-grammarly :recipe (:host github :repo "emacs-grammarly/lsp-grammarly" :files ("*")))

(package! lsp-bridge :recipe (:host github :repo "manateelazycat/lsp-bridge" :files ("*")))

(package! tabnine-capf :recipe (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*")))

(package! sort-tab :recipe (:host github :repo "manateelazycat/sort-tab" :files ("*")))

(package! highlight-symbol)

(package! ef-theme :recipe (:host github :repo "protesilaos/ef-themes" :files ("*")))

;;(package! telega :recipe (:host github :repo "zevlg/telega.el" :files ("*")) :pin "b581ac9")
(package! telega :recipe (:host github :repo "zevlg/telega.el" :files ("*")) :pin "123a8bccba7f773e3a0caaf97634681922854853")

(package! line-reminder :recipe (:host github :repo "emacs-vs/line-reminder"))

(package! elfeed)

(package! exec-path-from-shell)
(package! color-rg :recipe (:host github :repo "manateelazycat/color-rg" :files ("*")))

(package! pyim)

(package! emacsql-sqlite-builtin)

(package! flycheck-grammarly :recipe (:host github :repo "emacs-grammarly/flycheck-grammarly" :files ("*")))
