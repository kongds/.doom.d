;;; configs/ini-netease-music.el -*- lexical-binding: t; -*-

(use-package! netease-cloud-music
  :commands netease-cloud-music
  :config
  (require 'netease-cloud-music-ui)

  (setq netease-cloud-music-repeat-mode "playlist")

  (add-to-list 'evil-emacs-state-modes 'netease-cloud-music-mode)
  (define-key netease-cloud-music-mode-map (kbd "J") 'netease-cloud-music-storage-song)
  (define-key netease-cloud-music-mode-map (kbd "j") 'next-line)
  (define-key netease-cloud-music-mode-map (kbd "k") 'previous-line)
  (define-key netease-cloud-music-mode-map (kbd "G") 'end-of-buffer)
  (define-key netease-cloud-music-mode-map (kbd "K") 'netease-cloud-music-clear-playlist)


  (define-key netease-cloud-music-switch-playlist-mode-map (kbd "j") 'next-line)
  (define-key netease-cloud-music-switch-playlist-mode-map (kbd "k") 'previous-line)

  (define-key netease-cloud-music-switch-song-mode-map (kbd "j") 'next-line)
  (define-key netease-cloud-music-switch-song-mode-map (kbd "k") 'previous-line)

  (define-key netease-cloud-music-mode-map (kbd "C-w h") 'my-evil-move-left-window)
  (define-key netease-cloud-music-mode-map (kbd "C-w l") 'my-evil-move-right-window)
  (define-key netease-cloud-music-mode-map (kbd "C-w C-h") 'my-evil-move-left-window)
  (define-key netease-cloud-music-mode-map (kbd "C-w C-l") 'my-evil-move-right-window))
