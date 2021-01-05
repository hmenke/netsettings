;; -*- lexical-binding: t; -*-

;; Speed up the startup
(setq old/gc-cons-threshold gc-cons-threshold
      old/gc-cons-percentage gc-cons-percentage
      old/file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)
(defun user/reset-startup-values ()
  (setq gc-cons-threshold old/gc-cons-threshold
        gc-cons-percentage old/gc-cons-percentage
        file-name-handler-alist old/file-name-handler-alist))
(add-hook 'emacs-startup-hook 'user/reset-startup-values)

;; Auto save to emacs state dir
(setq user/auto-save-directory
      (file-name-as-directory
       (concat user-emacs-directory "auto-save-files")))
(unless (file-directory-p user/auto-save-directory)
  (make-directory user/auto-save-directory t))

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
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;
;; BUILTIN ;;
;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; These are not actually builtins but I just keep them here
  (when tool-bar-mode (tool-bar-mode -1))
  (when menu-bar-mode (menu-bar-mode -1))
  (when scroll-bar-mode (scroll-bar-mode -1))
  (tooltip-mode 0)

  ;; Enable some disabled commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  (setq-default
   indent-tabs-mode nil
   fill-column 80
   use-dialog-box nil)
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))
          " - Emacs"))

  ;; Don't write message to the minibuffer when it's active
  (when (boundp 'inhibit-message)
    (defun user/inhibit-message-in-minibuffer (f &rest args)
      (let ((inhibit-message (or inhibit-message (active-minibuffer-window))))
        (apply f args)))
    (advice-add 'message :around #'user/inhibit-message-in-minibuffer))

  ;; I don't like typing
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package "startup"
  :config
  (setq-default
   inhibit-startup-screen t
   initial-scratch-message ""
   auto-save-list-file-prefix user/auto-save-directory))

(use-package simple
  :bind ("M-SPC" . cycle-spacing)
  :config
  (setq-default
   line-number-mode t
   column-number-mode t
   visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
   async-shell-command-display-buffer nil
   async-shell-command-buffer 'new-buffer)
  (transient-mark-mode 1)
  (put 'overwrite-mode 'disabled t))

(use-package select
  :config
  (setq-default
   x-select-enable-clipboard t))

(use-package files
  :config
  (setq-default
   backup-by-copying t
   enable-local-eval 'maybe
   enable-local-variables t
   auto-save-file-name-transforms `((".*" ,user/auto-save-directory t))))

(use-package cus-edit
  :config
  (setq-default
   custom-file (concat user-emacs-directory "custom.el")))

;;(use-package comp
;;  :config
;;  (setq-default
;;   comp-deferred-compilation-black-list '("^/usr" "^/nix")
;;   comp-deferred-compilation t))

(use-package autorevert
  :config
  (global-auto-revert-mode t)
  (setq
   auto-revert-interval 2
   auto-revert-check-vc-info t
   global-auto-revert-non-file-buffers t
   auto-revert-verbose nil))

(use-package delsel
  :config
  (delete-selection-mode 1))

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; c++ mode enhancements
(use-package cc-mode
  :hook (c++-mode . (lambda() (c-set-offset 'innamespace 0)))
  :config
  (setq
   c-default-style "linux"
   c-basic-offset 4))

;; Use tabs for indentation in sh-mode
;; That play better with heredocs
(use-package sh-script
  :hook (sh-mode
         . (lambda()
             (setq indent-tabs-mode t
                   tab-width 8
                   sh-basic-offset 8
                   backward-delete-char-untabify-method nil))))

;; mouse integration
(use-package mouse
  :unless window-system
  :init
  ;; avoid spurious errors
  (defun track-mouse (e))
  :config
  (xterm-mouse-mode t)
  (setq mouse-sel-mode t))

;; Rebind C-z to avoid freezing
(use-package frame
  :bind (("C-z" . nil)
         ("C-z C-z" . user/suspend-frame)
         ("C-x C-z" . user/suspend-frame))
  :init
  (defun user/suspend-frame ()
    (interactive)
    (if (display-graphic-p)
        (error "Cannot suspend graphical frame")
      (suspend-frame)))
  :config
  (blink-cursor-mode 0))

;; Spell checking
;; https://emacs.stackexchange.com/questions/20206
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (flyspell-mode . user/flyspell-local-vars))
  :init
  (defun user/flyspell-local-vars ()
    (add-hook 'hack-local-variables-hook #'flyspell-buffer nil 'local))
  :config
   ;; default C-; is iedit
  (setq-default flyspell-auto-correct-binding [ignore]))

;; Dired enhancements
(use-package dired
  :commands dired
  :bind (("C-x C-j" . dired-jump)
         ("C-x j" . dired-jump-other-window)
         :map dired-mode-map
              ([mouse-2] . dired-mouse-find-file)
              ([M-up] . dired-up-directory)
              ([M-down] . dired-find-file)
              ("M-s O" . user/dired-multi-occur)
              ("M-t" . user/dired-open-in-terminal))
  :hook (dired-mode . dired-hide-details-mode)
  :init
  (setq user/dired-listing-switches " -laGh1 --group-directories-first")
  (defun user/dired-open-in-terminal ()
    (interactive)
    (let ((process-connection-type nil))
      (start-process-shell-command "xterm" "*Terminal*" "nohup xterm & exit")))
  (defun user/dired-multi-occur ()
    ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2009-12/msg00112.html
    "Search string in files marked by dired."
    (interactive
     (let ((files (dired-get-marked-files)))
       (if (null files) (error "No files marked")
         (let ((string (read-string "List lines matching regexp in marked files: ")))
           (multi-occur (mapcar 'find-file files) string))))))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq
   dired-listing-switches user/dired-listing-switches
   dired-use-ls-dired t
   dired-guess-shell-alist-user '((".*" "1>/dev/null 2>/dev/null nohup xdg-open"))
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
   ibuffer-show-empty-filter-groups nil
   ibuffer-saved-filter-groups '(("user"
                                  ("Files" (predicate . (buffer-file-name)))
                                  ("Dired" (mode . dired-mode))
                                  ("ERC" (mode . erc-mode))
                                  ("Magit" (or
                                            (mode . magit-mode)
                                            (name . "^magit")))
                                  ("Emacs" (name . "^\\*.*\\*$"))))
   ibuffer-expert t))

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
  :bind ("C-x C-r" . 'user/complete-recentf)
  :init
  (defun user/complete-recentf ()
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file (completing-read "Open recent: " files nil t))))
  :config
  (setq
   recentf-max-menu-items 25
   recentf-max-saved-items 25
   recentf-exclude '((expand-file-name package-user-dir)
                     "ido.*" "recentf"
                     ".gz" ".xz" ".zip")
   recentf-filename-handlers '(abbreviate-file-name)))

;; save position on exit
(use-package saveplace
  :config
  (if (< emacs-major-version 25)
      (setq-default save-place t)
    (save-place-mode)))

;; search
(use-package "isearch"
  :config
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t))

(when (< emacs-major-version 25)
  ;; icomplete (newer Emacs uses selectrum)
  (use-package icomplete
    :demand
    :after minibuffer
    :bind (:map icomplete-minibuffer-map
                ("<up>" . icomplete-backward-completions)
                ("C-p" . icomplete-backward-completions)
                ("<down>" . icomplete-forward-completions)
                ("C-n" . icomplete-forward-completions))
    :config
    (setq
     icomplete-delay-completions-threshold 100
     icomplete-max-delay-chars 2
     icomplete-compute-delay 0.0
     icomplete-show-matches-on-no-input t
     icomplete-with-completion-tables t
     icomplete-in-buffer t
     icomplete-tidy-shadowed-file-names nil
     icomplete-hide-common-prefix nil)
    (icomplete-mode 1)))

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
          ("\\`\\*TeX errors\\*\\'"
           (display-buffer-in-side-window)
           (window-height . 0.3)
           (side . bottom)
           (slot . 0))
          ("\\*Async Shell Command\\*.*"
           (display-buffer-no-window)
           nil)
          ("\\`\\*Org Agenda\\*\\'"
           (window-height . fit-window-to-buffer)
           nil)))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil))

(use-package winner
  :config (winner-mode))

;; tramp
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (add-to-list
   'backup-directory-alist
   (cons tramp-file-name-regexp
         (concat user-emacs-directory "tramp-backups/"))))

;; vc
(use-package vc
  :config
  (setq
   vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp)))

;; org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind ("C-c a" . org-agenda)
  :config
  (setq
   org-directory "/davs:henri@henrimenke.com:/webdav/"
   org-agenda-files '("/davs:henri@henrimenke.com:/webdav/")
   org-startup-folded nil
   org-completion-use-ido t
   org-export-html-validation-link nil
   org-html-validation-link nil))

(use-package org-capture
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        '(("t" "TODO" entry
           (file "TODO.org")
           "* TODO %?"))))

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

(use-package diminish
  :ensure t)

;; Vim bindings
(use-package evil
  :ensure t
  :commands evil-mode)

(unless (< emacs-major-version 25)
  ;; incremental completion
  (use-package selectrum
    :ensure t
    :config
    (selectrum-mode +1))
  (use-package selectrum-prescient
    :ensure t
    :config
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1))

  ;; magit
  (use-package magit
    :ensure t
    :defer 2
    :bind
    (("C-x g" . magit-status)
     ("C-x M-g" . magit-dispatch)
     ("C-c g" . magit-file-dispatch))))

(use-package diff-hl
  :ensure t
  :defer 2
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package xclip
  :unless window-system
  :if (executable-find "xclip")
  :ensure t
  :config
  (xclip-mode 1))

;; AUCTeX
(defun user/align-environment ()
  "Apply align to the current environment only."
  (interactive)
  (save-excursion
    (LaTeX-mark-environment)
    (align (point) (mark))
    (pop-mark)))

(use-package tex-keywords
  :load-path "lisp")

(use-package auctex
  :ensure t
  :mode (("\\.cls\\'" . LaTeX-mode)
         ("\\.dtx\\'" . LaTeX-mode)
         ("\\.sty\\'" . LaTeX-mode)
         ("\\.tex\\'" . LaTeX-mode)
         ("\\.mk\\(ii\\|iv\\|vi\\|xl\\|lx\\)\\'" . ConTeXt-mode)
         ("\\.mp\\(ii\\|iv\\|vi\\|xl\\|lx\\)\\'" . metapost-mode))
  :init
  ;; TeX mode enhancements
  (setq
   TeX-PDF-mode t
   TeX-quote-after-quote t
   TeX-parse-self t
   TeX-engine 'luatex
   TeX-command-Show "LaTeX"
   ;;TeX-view-program-selection '((output-pdf "Zathura"))
   TeX-source-correlate-start-server t
   TeX-parse-all-errors t
   ;;TeX-error-overview-open-after-TeX-run t
   TeX-debug-bad-boxes t
   TeX-debug-warnings t
   TeX-display-help 'expert
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

  ;; Don't fontify math
  (eval-after-load "font-latex"
    '(set-face-foreground 'font-latex-math-face nil))
  (setq font-latex-fontify-script nil)

  ;; Fontify primitives
  (setq font-latex-match-function-keywords
        (mapcar #'(lambda (primitive) (list primitive ""))
                tex-keywords/primitives))

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

(use-package bibref
  :load-path "lisp"
  :commands (bibref-from-doi
             bibref-from-arxiv))

;; bibtex
(use-package bibtex
  :bind (:map bibtex-mode-map
              ("C-c d" . bibref-from-doi)
              ("C-c x" . bibref-from-arxiv)
              ("C-c v" . bibtex-validate)
              ("C-c s" . bibtex-sort-buffer)
              ([down-mouse-3] . imenu))
  :config
  (setq
   bibtex-maintain-sorted-entries t))

;; direnv
(use-package direnv
  :ensure t
  :config (direnv-mode))

;; Language modes
(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))
(use-package clang-format
  :ensure t
  :bind (:map c++-mode-map ("C-M-<tab>" . user/clang-format))
  :init
  (defun user/clang-format (&optional style)
    (interactive)
    (let ((start (if (use-region-p) (region-beginning) (point-min)))
          (end (if (use-region-p) (region-end) (point-max)))
          (assume-file-name
           (if (file-remote-p buffer-file-name)
               (let ((local-file (file-local-name buffer-file-name)))
                 (if (file-readable-p local-file)
                     local-file
                   (expand-file-name (file-name-nondirectory buffer-file-name) "~")))
             buffer-file-name)))
      (clang-format-region start end style assume-file-name))))
(use-package cmake-mode
  :ensure t
  :mode ("\\`CMakeLists\\.txt\\'" "\\.cmake\\'"))
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

(unless (< emacs-major-version 25)
  (use-package blacken
    :ensure t
    :after python
    :bind (:map python-mode-map ("C-M-<tab>" . blacken-buffer))
    :commands blacken-buffer)
  (use-package haskell-mode
    :ensure t
    :mode "\\.hs\\'")
  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("\\`README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "pandoc"))
  (use-package nix-mode
    :ensure t
    :mode "\\.nix\\'")
  (use-package rust-mode
    :ensure t
    :mode "\\.rs\\'"))

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
  :diminish undo-tree-mode
  :ensure t
  :config
  (setq
   undo-tree-auto-save-history t
   undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))
   undo-tree-visualizer-diff nil)
  (global-undo-tree-mode t))

;; Show suggestions for incomplete key chords
(use-package which-key
  :diminish which-key-mode
  :ensure t
  :defer 2
  :config (which-key-mode))
