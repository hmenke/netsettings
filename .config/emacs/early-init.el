;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      max-lisp-eval-depth 1600)
(when (< emacs-major-version 29)
  (with-no-warnings
    (setq max-specpdl-size 2500)))
(defun user/reset-startup-values ()
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook #'user/reset-startup-values)

;; disable useless bars (from Doom Emacs)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; disable tooltips (from Doom Emacs)
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when (eq system-type 'gnu/linux)
  (setq x-gtk-use-system-tooltips nil))

;; package
(setq package-enable-at-startup nil
      package-quickstart t)
