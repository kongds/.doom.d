;;; configs/init-ts-mode.el -*- lexical-binding: t; -*-
(require 'python)
(require 'treesit)

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

(after! org
  ;; make `org-src-get-lang-mode' support `major-mode-remap-alist'
  (defun org-src-get-lang-mode (lang)
    "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
    (let ((mode (intern
                 (concat
                  (let ((l (or (cdr (assoc lang org-src-lang-modes)) lang)))
                    (if (symbolp l) (symbol-name l) l))
                  "-mode"))))
      (if (assoc mode major-mode-remap-alist)
          (cdr (assoc mode major-mode-remap-alist))
          mode)
    )))

(setq python-ts-mode-hook nil
      c-ts-mode-hook nil
      c++-ts-mode-hook nil
      json-ts-mode-hook nil
      bash-ts-mode-hook nil)

(add-hook 'python-ts-mode-hook #'(lambda () (run-hooks 'python-mode-hook)))
(add-hook 'c-ts-mode-hook #'(lambda () (run-hooks 'c-mode-hook)))
(add-hook 'c++-ts-mode-hook #'(lambda () (run-hooks 'c++-mode-hook)))
(add-hook 'json-ts-mode-hook #'(lambda () (run-hooks 'json-mode-hook)))
(add-hook 'bash-ts-mode-hook #'(lambda () (run-hooks 'sh-mode-hook)))

;; fontfiy
;; from https://github.com/gmlarumbe/emacsconf/blob/c9169e2b8ea4ef9dd29d27f111d1fc9c578d24e2/lisp-prog/python/python-ts-font-lock.el#L198
(defgroup python-ts-font-lock nil
  "Python tree-sitter faces."
  :group 'python)

(defface python-ts-punctuation-face
  '((default :inherit font-lock-keyword-face))
  "Face for punctuation symbols, e.g:
!,;:?'=<>*"
  :group 'python-ts-font-lock)


(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (c "https://github.com/uyha/tree-sitter-c")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq python--treesit-settings
      (append python--treesit-settings
              (treesit-font-lock-rules
               :language 'python
               :override t
               :feature 'keyword
               `([,@python--treesit-keywords "is not"] @font-lock-keyword-face
                 ["="  "." "," "+" "-" "*" "/" "//" "=="] @python-ts-punctuation-face) ; DANGER: Check brackets/operators/delimiters below

               :language 'python
               :override t
               :feature 'variable
               '((dotted_name) @default
                 ;;(keyword_argument name: (identifier) @font-lock-variable-name-face )
                 (keyword_argument name: (identifier) @font-lock-preprocessor-face )
                 )
               )))
