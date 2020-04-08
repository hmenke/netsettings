; Speed up the startup
(setq gc-cons-threshold-old gc-cons-threshold
      gc-cons-percentage-old gc-cons-percentage
      file-name-handler-alist-old file-name-handler-alist)
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)
(defun reset-startup-values ()
  (setq gc-cons-threshold gc-cons-threshold-old
        gc-cons-percentage gc-cons-percentage-old
        file-name-handler-alist file-name-handler-alist-old))
(add-hook 'emacs-startup-hook 'reset-startup-values)

; Actual configuration
(custom-set-variables '(inhibit-startup-screen t))
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)
(setq frame-title-format
      '("%b" (buffer-file-name " (%f)"
              (dired-directory (" ("
               (dired-directory dired-directory
                "") ")") "")) " - Emacs"))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-auto-revert-mode t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq x-select-enable-clipboard t)
(setq backup-by-copying t)
(setq-default indent-tabs-mode nil)
(setq visual-line-fringe-indicators
      '(left-curly-arrow right-curly-arrow))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")

; Show matching parentheses
(show-paren-mode 1)

; mouse integration
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

; package archives
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

; Vim bindings
(use-package evil
  :ensure t
  :commands evil-mode)

; magit
(use-package magit
  :defer 2
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-x M-d" . magit-dispatch-popup)))

; Language modes
(use-package auctex
  :ensure t
  :mode (("\\.cls\\'" . LaTeX-mode)
         ("\\.dtx\\'" . LaTeX-mode)
         ("\\.sty\\'" . LaTeX-mode)
         ("\\.tex\\'" . LaTeX-mode)
         ("\\.mkii\\'" . ConTeXt-mode)
         ("\\.mkiv\\'" . ConTeXt-mode)
         ("\\.mkvi\\'" . ConTeXt-mode)
         ("\\.mpii\\'" . metapost-mode)
         ("\\.mpiv\\'" . metapost-mode)
         ("\\.mpvi\\'" . metapost-mode))
  :init
  ; TeX mode enhancements
  (setq TeX-PDF-mode t)
  (setq TeX-quote-after-quote t)
  (setq-default TeX-engine 'luatex)
  (setq TeX-command-Show "LaTeX")
  (setq TeX-view-program-selection '((output-pdf "XDG")))
  (setq TeX-view-program-list '(("XDG" "xdg-open %o")))
  ; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-label-alist '(AMSTeX))
  (eval-after-load "font-latex"
    '(set-face-foreground 'font-latex-math-face nil))
  (setq font-latex-fontify-script nil)

  ; ConTeXt mode
  (setq ConTeXt-Mark-version "IV")
  (with-eval-after-load "context"
    (add-to-list 'TeX-file-extensions "mkiv" t)
    (add-to-list 'TeX-file-extensions "mkvi" t))
  (add-hook 'ConTeXt-mode-hook
            (lambda()
              (setq TeX-command-default "ConTeXt Full")
              (setq TeX-command-Show "ConTeXt Full"))))
(use-package modern-cpp-font-lock
  :ensure t
  :hook cpp-mode)
(use-package clang-format
  :ensure t
  :hook (c-mode cpp-mode))
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" "\\.cmake\\'"))
(use-package cuda-mode
  :ensure t
  :mode "\\.cu\\'")
(use-package cython-mode
  :ensure t
  :mode ("\\.pxd\\'" "\\.pyx\\'"))
(use-package d-mode
  :ensure t
  :mode "\\.d\\'")
(use-package gnuplot-mode
  :ensure t
  :mode "\\.gnuplot\\'")
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")
(use-package julia-mode
  :ensure t
  :mode "\\.jl\\'")
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config (setq lua-indent-level 4))
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")
(use-package rainbow-mode
  :ensure t)
(use-package dired-x
  :commands dired
  :init
  (setq dired-guess-shell-alist-user (list (list "\\.pdf$" "xdg-open")))
  (add-to-list 'display-buffer-alist
               (cons "\\*Async Shell Command\\*.*"
                     (cons #'display-buffer-no-window nil))))
(use-package notmuch
  :commands notmuch)
(use-package gnus
  :commands gnus
  :init
  (setq gnus-directory "~/.local/share/emacs/gnus"
        message-directory "~/.local/share/emacs/gnus")
  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port "imaps")
                 (nnimap-stream ssl)))
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)
  (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"))

; Theme
(use-package base16-theme
  :ensure t
  :config (load-theme 'base16-gruvbox-dark-hard t))

; c++ mode enhancements
(setq c-default-style "linux" c-basic-offset 4)
(c-set-offset 'innamespace 0)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
(defun enable-clang-format ()
  (require 'clang-format)
  (global-set-key [C-M-tab] 'clang-format-region))
(add-hook 'c-mode-hook 'enable-clang-format)
(add-hook 'c++-mode-hook 'enable-clang-format)

; message-mode enhancements
(setq message-kill-buffer-on-exit t)

; Dired enhancements
(setq dired-listing-switches
      "--group-directories-first -lh --hide=*~")
(with-eval-after-load 'dired
  (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)
  (define-key dired-mode-map [M-up] 'dired-up-directory)
  (define-key dired-mode-map [M-down] 'dired-find-file)
  (define-key dired-mode-map (kbd "M-t") 'dired-open-in-terminal))

(defun dired-open-in-terminal ()
  (interactive)
  (let ((process-connection-type nil))
    (start-process "" nil "x-terminal-emulator")
    ;(concat "--working-directory=" default-directory)
  ))

(defun dired-mouse-find-file (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
	    pos (posn-point (event-end event)))
      (if (not (windowp window))
	  (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (progn
	      (select-window window)
	    ; (dired-other-window file)))
	      (dired file)))
      (select-window window)
    ; (find-file-other-window (file-name-sans-versions file t)))))
      (find-file (file-name-sans-versions file t)))))
