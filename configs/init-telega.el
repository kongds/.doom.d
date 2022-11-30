;;; configs/init-telega.el -*- lexical-binding: t; -*-

(after! telega

  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'append)

  (add-hook 'telega-chat-mode-hook
            (lambda ()
              (setq-local doom-real-buffer-p t)))
  (add-hook 'telega-root-mode-hook
            (lambda ()
              (setq-local doom-real-buffer-p t)))

  (defun telega-open-chat-with-id (id)
    (telega-chat--pop-to-buffer (telega-chat-get id))
    (goto-char (point-max))
    (evil-insert-state)
    (if (and (featurep 'rime) (not rime-enable))
        (rime-toggle-input-method)))

  (setq telega-proxies
      (list
       '(:server "127.0.0.1" :port 1087 :enable t
                 :type (:@type "proxyTypeHttp")))))
