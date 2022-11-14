;;; doom-sort-tab.el -*- lexical-binding: t; -*-

(use-package! sort-tab
  :config

  (require 'consult)
  (defun sort-tab-workspace-buffer-list ()
    (cl-concatenate 'list
    (eaf--get-eaf-buffers)
    (mapcar #'get-buffer
            (consult--buffer-query :sort 'visibility
                                   :as #'buffer-name
                                   :predicate
                                   #'(lambda (buf)
                                       (let* ((workspace
                                               (and t
                                                    (+workspace-get (+workspace-current-name) t))))
                                         (if workspace
                                             (+workspace-contains-buffer-p
                                              buf workspace) nil)))))))
  (advice-add 'sort-tab-get-buffer-list
              :override #'(lambda ()
                            (let ((bufs (sort-tab-workspace-buffer-list)))
                              (setq bufs (cl-remove-if #'sort-tab-buffer-need-hide-p bufs))
                              (setq bufs (sort bufs #'sort-tab-buffer-freq-higher-p))
                              bufs)))


  (defun sort-tab-not-focus (&rest args)
    (if (eq (current-buffer) (sort-tab-get-buffer))
        (other-window 1)))

  (advice-add 'sort-tab-turn-off
              :after #'(lambda()
                         (advice-remove 'evil-window-up #'sort-tab-not-focus)
                         (advice-remove 'evil-window-down #'sort-tab-not-focus)
                         (advice-remove 'evil-window-left #'sort-tab-not-focus)
                         (advice-remove 'evil-window-right #'sort-tab-not-focus)
                         (advice-remove 'evil-window-next  #'sort-tab-not-focus)
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
                         (advice-add 'evil-window-up :after #'sort-tab-not-focus)
                         (advice-add 'evil-window-down :after #'sort-tab-not-focus)
                         (advice-add 'evil-window-left :after #'sort-tab-not-focus)
                         (advice-add 'evil-window-right :after #'sort-tab-not-focus)
                         (advice-add 'evil-window-next :after #'sort-tab-not-focus)
                         (advice-add '+workspace-switch
                                     :after #'(lambda (&rest r)
                                                (unless (get-buffer-window (sort-tab-get-buffer))
                                                  (sort-tab-create-window))))))


  (setq sort-tab-align 'center)
  (setq sort-tab-name-max-length 20)
)



(provide 'doom-sort-tab)
