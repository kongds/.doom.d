;;--- pyright -- 
(add-to-list 'load-path "~/.emacs.d/elpa/lsp-pyright")
(require 'lsp-pyright)
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          ;;(set (make-local-variable 'company-backends) '(company-capf)) ;company-tabnine))
                          (lsp))))  ; or lsp-deferred

;;--- lsp ---
(setq lsp-python-ms-executable
      "~/.emacs.d/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer")
(require 'lsp)
;;(require 'lsp-python-ms)
;;(setq lsp-python-ms-auto-install-server t)

;; redefine lsp-ui-doc--display to hide doc windows when doc string equal symbol
(after-load 'lsp-ui
	(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
	(define-key lsp-ui-mode-map (kbd "M-,") #'lsp-ui-peek-find-references)
  (defun lsp-ui-doc--display (symbol string)
    "Display the documentation."
    (when (and lsp-ui-doc-use-webkit (not (featurep 'xwidget-internal)))
      (setq lsp-ui-doc-use-webkit nil))
    (if (or (null string) (string-empty-p string) (string-equal symbol string))
        (lsp-ui-doc--hide-frame)
      (lsp-ui-doc--render-buffer string symbol)
      (if (lsp-ui-doc--inline-p)
          (lsp-ui-doc--inline)
        (unless (lsp-ui-doc--get-frame)
          (lsp-ui-doc--set-frame (lsp-ui-doc--make-frame)))
        (unless lsp-ui-doc-use-webkit
          (lsp-ui-doc--resize-buffer)
          (lsp-ui-doc--move-frame (lsp-ui-doc--get-frame)))
        (unless (frame-visible-p (lsp-ui-doc--get-frame))
          (make-frame-visible (lsp-ui-doc--get-frame)))))))

(require 'pip-requirements)
(require 'elpy)

(setenv "PATH" (concat (getenv "PATH") ":" "/Users/royokong/.local/bin"))
(setenv "PATH" (concat (getenv "PATH") ":" "~/.local/bin"))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
      ;;python-shell-interpreter-args "--pdb -i --simple-prompt")
(defun elpy-shell-send-file (file-name &optional process temp-file-name
                                         delete msg)
  "Like `python-shell-send-file' but evaluates last expression separately.

See `python-shell-send-file' for a description of the
arguments. This function differs in that it breaks up the
Python code in FILE-NAME into statements. If the last statement
is a Python expression, it is evaluated separately in 'eval'
mode. This way, the interactive python shell can capture (and
print) the output of the last expression."
  (interactive
   (list
    (read-file-name "File to send: ")   ; file-name
    nil                                 ; process
    nil                                 ; temp-file-name
    nil                                 ; delete
    t))                                 ; msg
  (let* ((process (or process (python-shell-get-process-or-error msg)))
         (encoding (with-temp-buffer
                     (insert-file-contents
                      (or temp-file-name file-name))
                     (python-info-encoding)))
         (file-name (expand-file-name
                     (or (file-remote-p file-name 'localname)
                         file-name)))
         (temp-file-name (when temp-file-name
                           (expand-file-name
                            (or (file-remote-p temp-file-name 'localname)
                                temp-file-name)))))
    (python-shell-send-string
     (format
      (concat
       "import sys, codecs, os, ast;"
       "__pyfile = codecs.open('''%s''', encoding='''%s''');"
       "__code = __pyfile.read().encode('''%s''');"
       "__pyfile.close();"
       (when (and delete temp-file-name)
         (format "os.remove('''%s''');" temp-file-name))
       "__block = ast.parse(__code, '''%s''', mode='exec');"
       "__last = __block.body[-1];" ;; the last statement
       "__isexpr = isinstance(__last,ast.Expr);" ;; is it an expression?
       "_ = __block.body.pop() if __isexpr else None;" ;; if so, remove it
       "exec(compile(__block, '''%s''', mode='exec'));" ;; execute everything else
       "eval(compile(ast.Expression(__last.value), '''%s''', mode='eval')) if __isexpr else None" ;; if it was an expression, it has been removed; now evaluate it
       )
      (or temp-file-name file-name) encoding encoding file-name file-name file-name)
     process)))

(setq elpy-rpc-python-command "/usr/local/bin/python")
;;(elpy-enable)

(define-key elpy-mode-map (kbd "M-/") #'company-capf)
;;(define-key elpy-mode-map (kbd "M-/") 'elpy-company-backend)

;;(when (maybe-require-package 'anaconda-mode)
;;  (after-load 'python
;;    (add-hook 'python-mode-hook 'anaconda-mode)
;;    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
;;  (when (maybe-require-package 'company-anaconda)
;;    (after-load 'company
;;      (add-hook 'python-mode-hook
;;                (lambda () (sanityinc/local-push-company-backend 'company-anaconda))))))


;;ein config
(add-to-list 'load-path "~/.emacs.d/elpa/ein-0.16.0")
(require 'ein)
(require 'ein-notebook)
(eval-after-load 'evil-mode
     (evil-define-key 'normal ein:notebook-mode-map (kbd "RET") 'ein:worksheet-execute-cell))

(provide 'init-python)
