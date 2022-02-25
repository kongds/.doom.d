(require 'evil)
(require 'dash-at-point)

(defun royokong-dash-at-point-run-search (search-string &optional docset)
  "Directly execute search for SEARCH-STRING in Dash."
  (start-process "Dash" nil
		 "/Applications/Dash.app/Contents/Resources/dashAlfredWorkflow"
		 (concat (when docset
			       (concat docset ":"))
			     search-string)))

(defun royokong-dash-at-point (&optional edit-search)
  "Search for the word at point in Dash.
If the optional prefix argument EDIT-SEARCH is specified,
the user will be prompted to edit the search string first."
  (interactive "P")
  (let* ((thing (thing-at-point 'symbol))
	 (docset (or dash-at-point-docset (dash-at-point-guess-docset))))
    (royokong-dash-at-point-run-search
     (if (or edit-search (null thing))
         (read-string "Dash search: " thing)
       thing)
     nil)))

(define-key evil-normal-state-map (kbd "K") (lambda ()
                                              (interactive)
                                              (let ((p (point))
                                                    (symbol (symbol-at-point))
                                                    (window (selected-window)))
                                                    (cond ((equal major-mode 'go-mode)
                                                           (godoc-at-point p))
                                                          ((equal major-mode 'python-mode)
                                                           (elpy-doc))
                                                          ((equal major-mode 'emacs-lisp-mode)
                                                             (helpful-callable symbol)
                                                             (select-window window)
                                                             )
                                                          (t (dash-at-point))
                                                    ))))


(provide 'init-dash)





