;;; pdf-search.el -*- lexical-binding: t; -*-

(after! pdf-tools
  (defun my/isearch-failed? ()
    (or (not isearch-success) isearch-error))

  (defvar-local my/pdf-isearch-highlight-matches nil)
  (defun my/pdf-isearch-cleanup-highlight ()
    (setq my/pdf-isearch-highlight-matches nil)
    (pdf-view-redisplay))

  ;; 模仿occur-hack-p，注入变量以控制高亮
  (defun my/pdf-isearch-hl-matches-controllable-highlight (orig-fun current matches &optional occur-hack-p)
    (funcall orig-fun current matches (or my/pdf-isearch-highlight-matches occur-hack-p)))
  (advice-add #'pdf-isearch-hl-matches
              :around #'my/pdf-isearch-hl-matches-controllable-highlight)

  (defvar-local my/pdf-isearch-forward? t)

  (defun my/pdf-isearch-forward (&optional regexp-p no-recursive-edit)
    (interactive "P\np")
    (setq my/pdf-isearch-forward? t)
    (isearch-forward regexp-p no-recursive-edit))

  (defun my/pdf-isearch-backward (&optional regexp-p no-recursive-edit)
    (interactive "P\np")
    (setq my/pdf-isearch-forward? nil)
    (isearch-backward regexp-p no-recursive-edit))

  ;; 下面参数arg，原接口是raw prefix，不是数字prefix，
  ;; 这里保持一致，我不知道怎么反转raw prefix（不用hack的方式，
  ;; 比如操作raw prefix列表），不然my/pdf-isearch-repeat-backward
  ;; 可以简单反转下prefix调用my/pdf-isearch-repeat-backward，而不用写两遍。
  (defun my/pdf-isearch-repeat-forward (&optional arg)
    (interactive "P")
    (setq my/pdf-isearch-highlight-matches t)
    (if my/pdf-isearch-forward?
        (isearch-repeat-forward arg)
      (isearch-repeat-backward arg)))

  (defun my/pdf-isearch-repeat-backward (&optional arg)
    (interactive "P")
    (setq my/pdf-isearch-highlight-matches t)
    (if my/pdf-isearch-forward?
        (isearch-repeat-backward arg)
      (isearch-repeat-forward arg)))

  ;; 搜索超过文档结尾时，直接wrap到文档头开始搜索，不要暂停，
  ;; 如果不这样设置，则搜索超过文档结尾时，暂停的时候，结尾的高亮会不见
  ;; 估计不难修复，但是这样搞更简单。
  (add-hook 'pdf-view-mode-hook
            (lambda () (setq-local isearch-wrap-pause 'no)))

  (defun my/pdf-view-force-normal-state ()
    (interactive)
    (evil-force-normal-state)
    (my/pdf-isearch-cleanup-highlight))

  (advice-add #'pdf-isearch-mode-initialize
              :before
              (lambda (&rest args)
                "在正常使用C-s等进行搜索的时候，重置 `my/pdf-isearch-highlight-matches'。"
                (setq my/pdf-isearch-highlight-matches nil)))

  (defun my/pdf-isearch-mode-cleanup ()
    "按/键搜索，如果搜索成功，则按回车后不要清除高亮，如果搜索失败，则清除高亮。"
    (pdf-isearch-active-mode -1)
    (when (my/isearch-failed?) (pdf-view-redisplay)))
  (advice-add #'pdf-isearch-mode-cleanup
              :override #'my/pdf-isearch-mode-cleanup)

  ;; 改成下面这段
  (defmacro my/modify-evil-collection-key-bindings (mode &rest body)
    (declare (indent defun))
    (let ((feature-name-var (gensym "feature-name"))
          (mode-var (gensym "mode"))
          (mode-str-var (gensym "mode-str"))
          (hook-fun-name-var (gensym "hook-fun-name-var")))
      `(let* ((,mode-var ,mode)
              (,mode-str-var (symbol-name ,mode-var))
              (,feature-name-var (intern (concat "evil-collection-" ,mode-str-var))))
         (if (featurep ,feature-name-var)
             ;; 在当前evil-collection的实现中，如果feature名对应的文件
             ;; 已经加载，则对应的按键setup函数也已经调用，故在这种情况下，
             ;; 我们可以直接执行body。
             (progn ,@body)
           ;; 否则，添加hook到`evil-collection-setup-hook'，
           ;; 判断mode名匹配后执行body。
           (let ((,hook-fun-name-var
                  ;; 这里使用gensym生成hook函数名，由于生成的符号是未intern的，
                  ;; 这会导致evil-collection无法调用我们的hook函数，于是我们需要
                  ;; 提前intern下，同时由于gensym会在符号名后面附加一个全局计数器，
                  ;; 加上我们的函数有前缀 my/ ，因此一般不会产生名称冲突。
                  (intern
                   (symbol-name
                    (gensym (concat "my/modify-evil-collection-key-bindings-"
                                    ,mode-str-var))))))
             ;; 使用defun底层使用的defalias，不然defun不会eval函数名参数。
             (defalias ,hook-fun-name-var
               (lambda (mode keymaps)
                 (when (eq mode ,mode-var)
                   ,@body))
               (concat "修改evil-collection设置的" ,mode-str-var "的按键绑定。"))
             (add-hook 'evil-collection-setup-hook ,hook-fun-name-var))))))

  (my/modify-evil-collection-key-bindings 'pdf
    (evil-define-key 'normal pdf-view-mode-map
      ;; 按/或者?开始向前或者向后搜索并记住搜索方向，
      ;; 按n/N继续向前/向后搜索，
      ;; 按ESC则返回normal state的同时清除搜索高亮
      (kbd "/") #'my/pdf-isearch-forward
      (kbd "?") #'my/pdf-isearch-backward
      (kbd "n") #'my/pdf-isearch-repeat-forward
      (kbd "N") #'my/pdf-isearch-repeat-backward
      (kbd "<escape>") #'my/pdf-view-force-normal-state))
  )

(after! pdf-outline
  (defun pdf-info-outline-format-to-list (buffer)
    "Convert outline format to list."
    (let* ((outline (pdf-info-outline buffer))
           (completions (make-hash-table :test 'equal :size (length outline)))
           (parent-section '()))
      (dolist (item outline)
        (let ((depth (cdr (nth 0 item)))
              (type (cdr (nth 1 item)))
              (title (cdr (nth 2 item)))
              (page (cdr (nth 3 item)))
              (top (cdr (nth 4 item))))
          (cond
           ((eq depth 1)
            (setq parent-section (list (cons title depth))))
           ((<= (length parent-section) (1- depth))
            (add-to-list 'parent-section (cons title depth) t))
           (t
            (setcdr (nthcdr (- depth 2) parent-section) (list (cons title depth)))))
          (message "parent-section: %s" parent-section)
          (puthash
           (if (equal depth 1)
               title
             (concat
              (let ((ptitle ""))
                (dolist (parent parent-section)
                  (when (> depth (cdr parent))
                    (message "parent: %s %s" parent ptitle)
                    (setq ptitle
                          (if (eq (length ptitle) 0)
                              (car parent)
                            (concat ptitle " > " (car parent))))))
                ptitle)
              " > " title))
           (list depth type page top title) completions)))
      completions))

  (defun pdf-outline-consult (section-title)
    (interactive (list
                  (completing-read "Section: "
                                   (pdf-info-outline-format-to-list (current-buffer)))))
    (let* ((section (gethash section-title (pdf-info-outline-format-to-list (current-buffer))))
           (depth (nth 0 section))
           (type (nth 1 section))
           (page (nth 2 section))
           (top (nth 3 section)))
      (pdf-links-action-perform
       `((depth . ,depth) (type . ,type) (title . ,section-title) (page . ,page) (top . ,top)))))

  (evil-collection-define-key 'normal 'pdf-view-mode-map
    "o" 'pdf-outline-consult))

(after! pdf-view
  (evil-collection-define-key 'normal 'pdf-view-mode-map
    "N" 'pdf-history-forward
    "B" 'pdf-history-backward))
