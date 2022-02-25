(add-to-list 'load-path "~/.emacs.d/elpa/fuz-20200104.524")

(require 'fuz)
(unless (require 'fuz-core nil t)
  (fuz-build-and-load-dymod))

(require 'helm-fuz)
(helm-fuz-mode)

(provide 'init-fuz)

(defun helm-fuzzy-matching-default-sort-fn (candidates _source)
  "Default `filtered-candidate-transformer' to sort candidates in fuzzy matching."
  (helm-fuzzy-matching-default-sort-fn-1 candidates))

(defun helm-fuz-fuzzy-ff-sort-candidate-advice! (orig-fun cands source)
  "Around advice where ORIG-FUN is `helm-ff-sort-candidates'.

Sign: (-> (-> (Listof Cand) Any (Listof Cand)) (Listof Cand) Any (Listof Cand))"
  (message "test")
  (cond ((string= (file-name-nondirectory helm-input) "")
         cands)
        ((string-match-p " " helm-pattern)
         (funcall orig-fun cands source))
        (t
         (helm-fuz-fuzzy-matching-sort-fn-1! helm-input
                                             cands
                                             #'helm-fuz--get-ff-cand-score-data))))

