(add-to-list 'load-path "/Users/royokong/.emacs.d/telega.el")
(require 'telega)
(setq telega-proxies (list '(:server "127.0.0.1" :port 1081 :enable t
                                     :type (:@type "proxyTypeSocks5"))))

(add-hook 'telega-chat-mode-hook (lambda ()
                                   (setq display-line-numbers nil)))
(add-hook 'telega-root-mode (lambda () 
                                   (setq display-line-numbers nil)))

(define-key telega-chat-mode-map (kbd "C-c C-r") #'telega-sticker-choose-favorite-or-recent)
(defun telega-mode-line-icon ()
  "T")
(add-hook 'telega-load-hook 'telega-mode-line-mode)

(provide 'init-telega)
