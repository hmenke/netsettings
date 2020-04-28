;; Speed up the startup
(setq gc-cons-threshold-old gc-cons-threshold
      gc-cons-percentage-old gc-cons-percentage
      file-name-handler-alist-old file-name-handler-alist)
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)
(defun user/reset-startup-values ()
  (setq gc-cons-threshold gc-cons-threshold-old
        gc-cons-percentage gc-cons-percentage-old
        file-name-handler-alist file-name-handler-alist-old))
(add-hook 'emacs-startup-hook 'user/reset-startup-values)

;; Actual configuration
(when tool-bar-mode (tool-bar-mode -1))
(when menu-bar-mode (menu-bar-mode -1))
(when scroll-bar-mode (scroll-bar-mode -1))
(tooltip-mode 0)
(blink-cursor-mode 0)

;; Enable some disabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Set defaults
(setq-default
 inhibit-startup-screen t
 line-number-mode t
 column-number-mode t
 x-select-enable-clipboard t
 backup-by-copying t
 indent-tabs-mode nil
 visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
 initial-scratch-message ""
 fill-column 80
 use-dialog-box nil
 enable-local-eval 'maybe
 enable-local-variables t)

(setq-default frame-title-format
      '("%b" (buffer-file-name " (%f)"
              (dired-directory (" ("
               (dired-directory dired-directory
                "") ")") "")) " - Emacs"))

(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(delete-selection-mode 1)

;; Auto save to emacs state dir
(setq auto-save-directory
      (file-name-as-directory
       (concat user-emacs-directory "auto-save-files")))
(unless (file-directory-p auto-save-directory)
  (make-directory auto-save-directory t))
(setq auto-save-file-name-transforms `((".*" ,auto-save-directory t))
      auto-save-list-file-prefix auto-save-directory)

;; Show matching parentheses
(show-paren-mode 1)

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; ibuffer
(defalias 'list-buffers 'ibuffer)
(setq
 ibuffer-expert t
 ibuffer-show-empty-filter-groups nil
 ibuffer-saved-filter-groups
 (quote (("user"
    ("Dired" (mode . dired-mode))
    ("ERC" (mode . erc-mode))
    ("Emacs" (or
              (name . "^\\*scratch\\*$")
              (name . "^\\*Messages\\*$")))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "user")))

;; mouse integration
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; package archives
(when (< emacs-major-version 27)
  (package-initialize))
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Clean up spaces
;; https://pages.sachachua.com/.emacs.d/Sacha.html
(bind-key "M-SPC" 'cycle-spacing)

;; Vim bindings
(use-package evil
  :ensure t
  :commands evil-mode)

;; magit
(use-package magit
  :defer 2
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c g" . magit-file-dispatch)))

;; AUCTeX
(defun user/align-environment ()
  "Apply align to the current environment only."
  (interactive)
  (save-excursion
    (LaTeX-mark-environment)
    (align (point) (mark))
    (pop-mark)))

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
  ;; TeX mode enhancements
  (setq
   TeX-PDF-mode t
   TeX-quote-after-quote t
   TeX-parse-self t
   TeX-engine 'luatex
   TeX-command-Show "LaTeX"
   TeX-source-correlate-start-server t
   ;; TeX-auto-local nil
   ;; TeX-auto-save t
   )

  ;; Key bindings
  (defun user/LaTeX-mode-hook ()
    (defalias 'align-environment 'user/align-environment)
    (define-key LaTeX-mode-map "\C-ca" 'align-environment)
    (define-key LaTeX-mode-map [down-mouse-3] 'imenu))
  (add-hook 'LaTeX-mode-hook 'user/LaTeX-mode-hook)

  ;; SyncTeX
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

  ;; RefTeX
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
  (add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
  (setq
   reftex-plug-into-AUCTeX t
   reftex-label-alist '(AMSTeX)
   reftex-insert-label-flags '("s" t))

  ;; Don't fontify
  (eval-after-load "font-latex"
    '(set-face-foreground 'font-latex-math-face nil))
  (setq font-latex-fontify-script nil)

  ;; Override default indentation
  (setq LaTeX-indent-environment-list
   (quote
    (("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("filecontents" current-indentation)
     ("filecontents*" current-indentation))))

  ;; ConTeXt mode
  (setq ConTeXt-Mark-version "IV")
  (with-eval-after-load "context"
    (add-to-list 'TeX-file-extensions "mkiv" t)
    (add-to-list 'TeX-file-extensions "mkvi" t))
  (add-hook 'ConTeXt-mode-hook
            (lambda()
              (setq TeX-command-default "ConTeXt Full"
                    TeX-command-Show "ConTeXt Full"))))

;; Language modes
(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))
(use-package clang-format
  :ensure t
  :commands (clang-format clang-format-region clang-format-buffer))
(use-package cmake-mode
  :ensure t
  :mode ("\\`CMakeLists.txt\\'" "\\.cmake\\'"))
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
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")
(use-package julia-mode
  :ensure t
  :mode "\\.jl\\'")
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :config (setq lua-indent-level 4))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")
(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode))
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

;; Editing plugins
(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)))
(use-package iedit
  :ensure t
  :bind
  (("C-;" . iedit-mode)))

;; Show suggestions for incomplete key chords
(use-package which-key
  :ensure t
  :defer 2
  :config (which-key-mode))

;; Replacement for DocView
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query :skip-dependencies))

;; Theme
(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox-dark-hard t))

(use-package telephone-line
  :ensure t
  :if window-system
  :config (telephone-line-mode 1))

;; c++ mode enhancements
(setq c-default-style "linux" c-basic-offset 4)
(add-hook 'c++-mode-hook
          (lambda() (c-set-offset 'innamespace 0)))

;; Use tabs for indentation in sh-mode
;; That play better with heredocs
(add-hook 'sh-mode-hook
          (lambda()
            (setq indent-tabs-mode t
                  tab-width 8
                  sh-basic-offset 8
                  backward-delete-char-untabify-method nil)))

;; Spell checking
;; https://emacs.stackexchange.com/questions/20206
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'flyspell-mode-hook #'user/flyspell-local-vars)
(defun user/flyspell-local-vars ()
  (add-hook 'hack-local-variables-hook #'flyspell-buffer nil 'local))
(setq-default flyspell-auto-correct-binding [ignore]) ;; default C-; is iedit

;; Do not display the buffer for async shell commands
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*.*" display-buffer-no-window nil))

;; message-mode enhancements
(setq message-kill-buffer-on-exit t)

;; DocView enhancements
(setq doc-view-resolution 160)

;; Dired enhancements
(setq
 dired-listing-switches "--group-directories-first -lah"
 dired-guess-shell-alist-user '(("\\.pdf\\'" "xdg-open"))
 dired-auto-revert-buffer t
 dired-dwin-target t)

(with-eval-after-load 'dired
  (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)
  (define-key dired-mode-map [M-up] 'dired-up-directory)
  (define-key dired-mode-map [M-down] 'dired-find-file)
  (define-key dired-mode-map (kbd "M-t") 'user/dired-open-in-terminal)
  (require 'dired-x)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\.+$\\|^\\..+$"))
  (setq dired-omit-verbose nil))

(add-hook 'dired-mode-hook 'dired-omit-mode)

(defun user/dired-open-in-terminal ()
  (interactive)
  (let ((process-connection-type nil))
    (start-process-shell-command "xterm" "*Terminal*" "nohup xterm & exit")))
