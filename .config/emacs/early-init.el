;; -*- lexical-binding: t; -*-

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
(setq package-enable-at-startup nil)
