;;; doom-sort-tab.el -*- lexical-binding: t; -*-

(use-package! sort-tab
  :config
  (require 'consult)
  (defun sort-tab-workspace-buffer-list ()
    (mapcar #'get-buffer (consult--buffer-query :sort 'visibility
                                                :as #'buffer-name
                                                :predicate #'(lambda (buf)
                                                               (let* ((workspace (and t
                                                                                      (+workspace-get
                                                                                       (+workspace-current-name)
                                                                                       t))))
                                                                 (if workspace
                                                                     (+workspace-contains-buffer-p
                                                                      buf workspace) nil))))))

  (advice-add 'sort-tab-get-buffer-list
              :override #'(lambda ()
                            (let ((bufs (sort-tab-workspace-buffer-list)))
                              (setq bufs (cl-remove-if #'sort-tab-buffer-need-hide-p bufs))
                              (setq bufs (sort bufs #'sort-tab-buffer-freq-higher-p))
                              bufs)))

  (advice-add 'sort-tab-turn-off
              :after #'(lambda()
                         (advice-remove '+workspace-switch #'(lambda (&rest r)
                                                               (unless (get-buffer-window (sort-tab-get-buffer))
                                                                 (sort-tab-create-window))))))
  (advice-add 'sort-tab-turn-on
              :after #'(lambda()
                         (map!
                          (:leader
                           :desc "1" :n "1" #'sort-tab-select-visible-tab
                           :desc "2" :n "2" #'sort-tab-select-visible-tab
                           :desc "3" :n "3" #'sort-tab-select-visible-tab
                           :desc "4" :n "4" #'sort-tab-select-visible-tab
                           :desc "5" :n "5" #'sort-tab-select-visible-tab
                           :desc "6" :n "6" #'sort-tab-select-visible-tab
                           :desc "7" :n "7" #'sort-tab-select-visible-tab
                           :desc "8" :n "8" #'sort-tab-select-visible-tab
                           :desc "9" :n "9" #'sort-tab-select-visible-tab
                           ))
                         (advice-add '+workspace-switch
                                     :after #'(lambda (&rest r)
                                                (unless (get-buffer-window (sort-tab-get-buffer))
                                                  (sort-tab-create-window))))))
)

(provide 'doom-sort-tab)
