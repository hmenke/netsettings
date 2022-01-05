;; -*- lexical-binding: t; -*-

;; disable useless bars
(when (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(when (bound-and-true-p menu-bar-mode) (menu-bar-mode -1))
(when (bound-and-true-p scroll-bar-mode) (scroll-bar-mode -1))

;; package
(setq package-enable-at-startup nil)
