;; -*- lexical-binding: t; -*-

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
 enable-local-variables t
 custom-file (concat user-emacs-directory "custom.el")
 async-shell-command-display-buffer nil)

(setq-default frame-title-format
      '("%b" (buffer-file-name " (%f)"
              (dired-directory (" ("
               (dired-directory dired-directory
                "") ")") "")) " - Emacs"))

(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(delete-selection-mode 1)
(transient-mark-mode 1)
(show-paren-mode 1)

;; Auto save to emacs state dir
(setq auto-save-directory
      (file-name-as-directory
       (concat user-emacs-directory "auto-save-files")))
(unless (file-directory-p auto-save-directory)
  (make-directory auto-save-directory t))
(setq auto-save-file-name-transforms `((".*" ,auto-save-directory t))
      auto-save-list-file-prefix auto-save-directory)

;; Clean up spaces
;; https://pages.sachachua.com/.emacs.d/Sacha.html
(global-set-key [?\M- ] 'cycle-spacing)
(global-set-key [?\M-/] 'hippie-expand)

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

;; message-mode enhancements
(setq message-kill-buffer-on-exit t)

;; DocView enhancements
(setq doc-view-resolution 160)

;; Narrow/widen dwim
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;
;;;; PACKAGES ;;;;
;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;

;; package archives
(setq package-enable-at-startup nil
      package--init-file-ensured t)
(when (< emacs-major-version 27)
  (package-initialize))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;
;; BUILTIN ;;
;;;;;;;;;;;;;

;; mouse integration
(defun track-mouse (e)) ;; avoid spurious errors
(use-package mouse
  :unless window-system
  :config
  (xterm-mouse-mode t)
  (setq mouse-sel-mode t))

;; Dired enhancements
(setq user/dired-listing-switches " -laGh1 --group-directories-first")
(defun user/dired-open-in-terminal ()
  (interactive)
  (let ((process-connection-type nil))
    (start-process-shell-command "xterm" "*Terminal*" "nohup xterm & exit")))

(use-package dired
  :commands dired
  :bind (("C-x C-j" . dired-jump)
         ("C-x j" . dired-jump-other-window)
         :map dired-mode-map
              ([mouse-2] . dired-mouse-find-file)
              ([M-up] . dired-up-directory)
              ([M-down] . dired-find-file)
              ("M-t" . user/dired-open-in-terminal))
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq
   dired-listing-switches user/dired-listing-switches
   dired-use-ls-dired t
   dired-guess-shell-alist-user '(("\\.pdf\\'" "xdg-open"))
   dired-auto-revert-buffer t
   dired-dwin-target t))

(use-package dired-x
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\.+$\\|^\\..+$"))
  (setq dired-omit-verbose nil))

(use-package dired-aux
  :after dired
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t))

(use-package find-dired
  :after dired
  :config
  (setq find-ls-option `("-ls" . ,user/dired-listing-switches))
  (setq find-name-arg "-iname"))

;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :hook ((ibuffer-mode . ibuffer-auto-mode)
         (ibuffer-mode . (lambda ()
                           (ibuffer-switch-to-saved-filter-groups "user"))))
  :config
  (setq
   ibuffer-expert t
   ibuffer-show-empty-filter-groups nil
   ibuffer-saved-filter-groups
   '(("user"
      ("Dired" (mode . dired-mode))
      ("ERC" (mode . erc-mode))
      ("Magit" (or
                (mode . magit-mode)
                (name . "^magit")))
      ("Emacs" (or
                (name . "^\\*scratch\\*$")
                (name . "^\\*Messages\\*$")))))))

;; minibuffer
(use-package minibuffer
  :config
  (setq
   enable-recursive-minibuffers t
   completions-format 'vertical
   completion-category-defaults nil
   read-file-name-completion-ignore-case t
   read-buffer-completion-ignore-case t
   completion-ignore-case t)
  (add-to-list 'completion-styles 'substring)
  (add-to-list 'completion-styles 'initials))

;; save minibuffer history
(use-package savehist
  :config
  (setq
   savehist-file (concat user-emacs-directory "savehist")
   history-length 30000
   history-delete-duplicates t
   savehist-save-minibuffer-history t)
  (savehist-mode 1))

;; save list of recent files
(use-package recentf
  :hook (after-init . recentf-mode)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (setq
   recentf-max-menu-items 25
   recentf-max-saved-items 25
   recentf-exclude '((expand-file-name package-user-dir)
                     "ido.*" "recentf"
                     ".gz" ".xz" ".zip")
   recentf-filename-handlers '(abbreviate-file-name)))

;; ido mode
(defun user/ido-vertical-define-keys ()
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match))
(defun user/ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(defun user/ido-complete-execute-extended-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read "M-x " (all-completions "" obarray 'commandp)))))
(use-package ido
  :defer 0.1
  :after minibuffer
  :bind ("M-x" . 'user/ido-complete-execute-extended-command)
  :config
  (setq
   ido-enable-flex-matching t
   ido-everywhere t
   ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                     " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")
   ido-default-file-method 'selected-window
   ido-default-buffer-method 'selected-window
   ido-create-new-buffer 'prompt
   ido-confirm-unique-completion nil
   ido-completion-buffer-all-completions nil
   ido-use-virtual-buffers t)
  (add-hook 'ido-minibuffer-setup-hook 'user/ido-disable-line-truncation)
  (add-hook 'ido-setup-hook 'user/ido-vertical-define-keys)
  (ido-mode 1))

;; icomplete
;;(use-package icomplete
;;  :demand
;;  :after minibuffer
;;  :bind (:map icomplete-minibuffer-map
;;              ("<up>" . icomplete-backward-completions)
;;              ("C-p" . icomplete-backward-completions)
;;              ("<down>" . icomplete-forward-completions)
;;              ("C-n" . icomplete-forward-completions))
;;  :config
;;  (setq
;;   icomplete-delay-completions-threshold 100
;;   icomplete-max-delay-chars 2
;;   icomplete-compute-delay 0.0
;;   icomplete-show-matches-on-no-input t
;;   icomplete-with-completion-tables t
;;   icomplete-in-buffer t
;;   icomplete-tidy-shadowed-file-names nil
;;   icomplete-hide-common-prefix nil)
;;  (icomplete-mode 1))

;; window customizations
(use-package "window"
  :config
  (setq display-buffer-alist
        '(("\\`\\*.*Completions.*\\*\\'"
           (display-buffer-in-side-window)
           (window-height . 0.3)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*Async Shell Command\\*.*"
           (display-buffer-no-window)
           nil)))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil))

(use-package windmove
  :bind (("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down)))

;; tramp
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (add-to-list
   'backup-directory-alist
   (cons tramp-file-name-regexp
         (concat user-emacs-directory "tramp-backups/"))))

;; org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind ("C-c a" . org-agenda)
  :config
  (setq
   org-directory "/davs:henri@henrimenke.com:/webdav/"
   org-agenda-files '("/davs:henri@henrimenke.com:/webdav/")
   org-completion-use-ido t))

(use-package org-capture
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        '(("t" "TODO" entry
           (file "TODO.org")
           "* TODO %?"))))

;; email
(use-package notmuch
  :commands notmuch)

;; gnus
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

;;;;;;;;;;;
;; MELPA ;;
;;;;;;;;;;;

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
   TeX-view-program-selection '((output-pdf "Zathura"))
   TeX-source-correlate-start-server t
   ;; TeX-auto-local nil
   ;; TeX-auto-save t
   LaTeX-reftex-cite-format-auto-activate nil)

  ;; Key bindings
  (defun user/LaTeX-mode-hook ()
    (defalias 'align-environment 'user/align-environment)
    (define-key LaTeX-mode-map "\C-ca" 'align-environment)
    (define-key LaTeX-mode-map "\C-xn" nil) ;; narrow-or-widen-dwim
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
   reftex-insert-label-flags '("s" t)
   reftex-cite-format 'default
   reftex-cite-key-separator ", ")

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
(use-package blacken
  :ensure t
  :commands blacken-buffer)
(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))
(use-package clang-format
  :ensure t
  :commands (clang-format clang-format-region clang-format-buffer))
(use-package cmake-mode
  :ensure t
  :mode ("\\`CMakeLists.txt\\'" "\\.cmake\\'"))
(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")
(use-package cuda-mode
  :ensure t
  :mode "\\.cu\\'")
(use-package cython-mode
  :ensure t
  :mode ("\\.pxd\\'" "\\.pyx\\'"))
(use-package d-mode
  :ensure t
  :mode "\\.d\\'")
(use-package gnuplot
  :ensure t
  :mode ("\\.gnuplot\\'" . gnuplot-mode)
  :config (setq gnuplot-display-process nil))
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

;; Editing plugins
(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)))
(use-package iedit
  :ensure t
  :bind
  (("C-;" . iedit-mode)))
(use-package undo-tree
  :ensure t
  :config
  (setq
   undo-tree-auto-save-history t
   undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))
   undo-tree-visualizer-diff nil)
  (global-undo-tree-mode t))

;; ivy, counsel, swiper
;;(use-package ivy
;;  :ensure t
;;  :defer 0.1
;;  :config
;;  (ivy-mode 1)
;;  (setq ivy-use-virtual-buffers t)
;;  (setq enable-recursive-minibuffers t))
;;
;;(use-package counsel
;;  :ensure t
;;  :after ivy
;;  :config
;;  (counsel-mode 1))
;;
;;(use-package swiper
;;  :ensure t
;;  :after ivy
;;  :bind (("C-s" . swiper)))

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
  :config (load-theme 'gruvbox t)
  ;; https://github.com/greduan/emacs-theme-gruvbox/pull/160
  (add-hook
   'ido-setup-hook
   (lambda ()
     (set-face-attribute 'ido-only-match nil
                         :foreground (face-attribute 'success :foreground)
                         :weight 'bold)
     (set-face-attribute 'ido-first-match nil
                         :foreground (face-attribute 'default :foreground)
                         :weight 'bold
                         :underline t)
     (set-face-attribute 'ido-subdir nil
                         :foreground (face-attribute 'font-lock-function-name-face :foreground))
     )))

(use-package telephone-line
  :ensure t
  :if window-system
  :config (telephone-line-mode 1))
