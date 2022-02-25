;;; +patches.el -*- lexical-binding: t; -*-

(defun projectile-compilation-buffer-name (compilation-mode)
  "Meant to be used for `compilation-buffer-name-function`.
Argument COMPILATION-MODE is the name of the major mode used for the compilation buffer."
  (concat "*" (downcase compilation-mode) "*"
          (if (projectile-project-p) (concat "<" (projectile-project-name) ">") "")))
