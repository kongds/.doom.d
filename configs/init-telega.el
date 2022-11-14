;;; configs/init-telega.el -*- lexical-binding: t; -*-

(after! telega
  (add-hook 'telega-chat-mode-hook
            (lambda ()
              (setq-local doom-real-buffer-p t)))
  (add-hook 'telega-root-mode-hook
            (lambda ()
              (setq-local doom-real-buffer-p t)))

  (setq telega-proxies
      (list
       '(:server "127.0.0.1" :port 1087 :enable t
                 :type (:@type "proxyTypeHttp")))))
