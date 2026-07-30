;; -*- lexical-binding: t; -*-
(when (< emacs-major-version 27)
  (error "Emacs is too old!"))

;; Focus hooks
(defun user/focus-in-hook())
(defun user/focus-out-hook()
  (garbage-collect))
(defun user/after-focus-change-function ()
  (if (frame-focus-state)
      (user/focus-in-hook)
    (user/focus-out-hook)))
(add-function :after after-focus-change-function #'user/after-focus-change-function)

;; Narrow/widen dwim
;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (declare (interactive-only t))
  (interactive "P")
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

;; Enable some disabled commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq-default
 indent-tabs-mode nil
 fill-column 80)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - Emacs"))

;; Don't write message to the minibuffer when it's active
(defun user/inhibit-message-in-minibuffer (f &rest args)
  (let ((inhibit-message (or inhibit-message (active-minibuffer-window))))
    (apply f args)))
(advice-add 'message :around #'user/inhibit-message-in-minibuffer)

;; I don't like typing
(if (boundp 'use-short-answers)         ; 28+
    (setq use-short-answers t)
  (defalias 'yes-or-no-p 'y-or-n-p))

;; startup
(setq-default
 inhibit-startup-screen t
 initial-scratch-message "")

;; mule (from Doom Emacs)
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; simple
(setq-default
 line-number-mode t
 column-number-mode t
 visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
 async-shell-command-display-buffer nil
 async-shell-command-buffer 'new-buffer)
(put 'overwrite-mode 'disabled t)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
;; Clashes with undo-tree
;;(define-key global-map [remap undo] 'undo-only)
;;(global-set-key (kbd "M-_") 'undo-redo) ;; backported from Emacs 28

;; backport undo-redo from Emacs 28
;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2645ae1222db1df270276b227e5102884466ecb0
(when (< emacs-major-version 28)
  (defun undo--last-change-was-undo-p (undo-list)
    (while (and (consp undo-list) (eq (car undo-list) nil))
      (setq undo-list (cdr undo-list)))
    (gethash undo-list undo-equiv-table))

  (defun undo-redo (&optional arg)
    "Undo the last ARG undos."
    (interactive "*p")
    (cond
     ((not (undo--last-change-was-undo-p buffer-undo-list))
      (user-error "No undo to undo"))
     (t
      (let* ((ul buffer-undo-list)
             (new-ul
              (let ((undo-in-progress t))
                (while (and (consp ul) (eq (car ul) nil))
                  (setq ul (cdr ul)))
                (primitive-undo arg ul)))
             (new-pul (undo--last-change-was-undo-p new-ul)))
        (message "Redo%s" (if undo-in-region " in region" ""))
        (setq this-command 'undo)
        (setq pending-undo-list new-pul)
        (setq buffer-undo-list new-ul))))))

;; files
(setq-default backup-by-copying t)
(let ((auto-save-directory (file-name-as-directory (locate-user-emacs-file "auto-save-files")))
      (backup-directory (file-name-as-directory (locate-user-emacs-file "backups"))))
  (unless (file-directory-p auto-save-directory)
    (make-directory auto-save-directory t))
  (unless (file-directory-p backup-directory)
    (make-directory backup-directory t))
  (setq-default
   backup-directory-alist `(("\\`/dev/shm") ("." . ,backup-directory))
   auto-save-file-name-transforms `((".*" ,auto-save-directory t))))
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31908
(setq-default create-lockfiles nil)

;; cus-edit
(setq custom-file (locate-user-emacs-file "custom.el"))

;; comp
(if (boundp 'native-comp-jit-compilation)
    (setq native-comp-jit-compilation t
          native-comp-jit-compilation-deny-list '("^/usr" "^/nix"))
  (with-no-warnings
    (setq comp-deferred-compilation t
          comp-deferred-compilation-black-list '("^/usr" "^/nix"))))

;; completion
(with-eval-after-load 'dabbrev
  (when (fboundp 'dabbrev-capf)
    (add-hook 'completion-at-point-functions #'dabbrev-capf 100)))
(when (fboundp 'global-completion-preview-mode)
  (global-completion-preview-mode 1)
  (define-key completion-preview-active-mode-map (kbd "M-n") #'completion-preview-next-candidate)
  (define-key completion-preview-active-mode-map (kbd "M-p") #'completion-preview-prev-candidate))

;; autorevert
(setq
 auto-revert-interval 2
 auto-revert-avoid-polling t
 auto-revert-check-vc-info t
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; delsel
(delete-selection-mode 1)

;; paren
(setq show-paren-delay 0)
(show-paren-mode 1)

;; hippie-exp
(setq
 hippie-expand-try-functions-list
 '(try-expand-dabbrev
   try-expand-dabbrev-all-buffers
   try-expand-dabbrev-from-kill
   try-expand-all-abbrevs
   try-expand-list
   try-expand-line
   try-complete-file-name-partially
   try-complete-file-name
   try-complete-lisp-symbol-partially
   try-complete-lisp-symbol)
 hippie-expand-verbose t
 hippie-expand-dabbrev-skip-space nil
 hippie-expand-dabbrev-as-symbol t
 hippie-expand-no-restriction t)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; cc-mode
(defun user/c-mode-common-hook ()
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (c-set-offset 'innamespace 0))
(add-hook 'c-mode-common-hook #'user/c-mode-common-hook)
(setq
 c-default-style "linux"
 c-basic-offset 2)

;; Use tabs for indentation in sh-mode
;; That play better with heredocs
;; NOTE: `sh-basic-offset' and `backward-delete-char-untabify-method' are not
;; automatically buffer-local, hence `setq-local'.
(define-minor-mode user/indent-tabs-mode
  "Use tabs for indentation"
  :init-value nil
  :lighter " Tabs"
  (if user/indent-tabs-mode
      (setq-local indent-tabs-mode t
                  tab-width 8
                  sh-basic-offset 8
                  backward-delete-char-untabify-method nil)
    (kill-local-variable 'indent-tabs-mode)
    (kill-local-variable 'tab-width)
    (kill-local-variable 'sh-basic-offset)
    (kill-local-variable 'backward-delete-char-untabify-method)))
(add-hook 'sh-mode-hook 'user/indent-tabs-mode)

;; mouse
;; `mouse-wheel-scroll-amount' has a :set function that reinstalls the bindings,
;; so it needs `customize-set-variable' rather than `setq'.
(xterm-mouse-mode 1)
(customize-set-variable 'mouse-wheel-scroll-amount
                        '(5
                          ((shift) . hscroll)
                          ((meta) . nil)
                          ((control) . text-scale)))

;; pixel-scroll
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; frame
(blink-cursor-mode 0)
;; set proper theme background for tmux
(setq frame-background-mode 'light)
;; Rebind C-z to avoid freezing
(defun user/suspend-frame ()
  (interactive)
  (if (display-graphic-p)
      (error "Cannot suspend graphical frame")
    (suspend-frame)))
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") 'user/suspend-frame)

;; flyspell
;; https://emacs.stackexchange.com/questions/20206
(defun user/flyspell-local-vars ()
  (add-hook 'hack-local-variables-hook #'flyspell-buffer nil 'local))
(add-hook 'flyspell-mode-hook 'user/flyspell-local-vars)

;; shell
(setq-default shell-kill-buffer-on-exit t)

;; dired
(autoload 'dired-jump "dired-x")
(define-key ctl-x-map "\C-j" 'dired-jump)
(defvar user/dired-listing-switches "-ahl --group-directories-first")

(with-eval-after-load "dired"
  (defun user/dired-open-in-terminal ()
    (interactive)
    (let ((process-connection-type nil))
      (start-process-shell-command "xterm" "*Terminal*" "nohup xterm & exit")))
  (defun user/dired-multi-occur (regexp)
    "Show lines matching REGEXP in the files marked in dired."
    ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2009-12/msg00112.html
    (interactive "sList lines matching regexp in marked files: ")
    (let ((files (dired-get-marked-files)))
      (unless files (user-error "No files marked"))
      (multi-occur (mapcar #'find-file files) regexp)))
  (put 'dired-find-alternate-file 'disabled nil)
  (setq
   dired-listing-switches user/dired-listing-switches
   dired-use-ls-dired t
   dired-guess-shell-alist-user '((".*" "1>/dev/null 2>/dev/null nohup xdg-open"))
   dired-auto-revert-buffer t
   dired-dwim-target t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)
  (define-key dired-mode-map [M-up] 'dired-up-directory)
  (define-key dired-mode-map [M-down] 'dired-find-file)
  (define-key dired-mode-map (kbd "M-s O") 'user/dired-multi-occur)
  (define-key dired-mode-map (kbd "M-t") 'user/dired-open-in-terminal)

  ;; dired-x
  (require 'dired-x)
  (setq
   dired-omit-files (concat dired-omit-files "\\|\\`\\.")
   dired-omit-verbose nil)
  (add-hook 'dired-mode-hook 'dired-omit-mode)

  ;; dired-aux
  (require 'dired-aux)
  (setq
   dired-isearch-filenames 'dwim
   dired-create-destination-dirs 'ask
   dired-vc-rename-file t)

  ;; find-dired
  (require 'find-dired)
  (setq
   find-ls-option `("-ls" . ,user/dired-listing-switches)
   find-name-arg "-iname"))

;; ibuffer
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
 ibuffer-expert t)
(defun user/ibuffer-mode-hook ()
  (ibuffer-switch-to-saved-filter-groups "user"))
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(add-hook 'ibuffer-mode-hook #'user/ibuffer-mode-hook)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; minibuffer
(setq
 completion-auto-help t
 completion-auto-select nil
 completion-category-defaults nil
 completion-eager-display t
 completion-eager-update t
 completion-ignore-case t
 completion-show-help nil
 completion-show-inline-help nil
 completion-styles '(basic partial-completion substring initials flex)
 completion-category-overrides '((file . ((styles partial-completion))))
 completions-detailed t
 completions-format 'one-column
 completions-max-height 12
 completions-sort 'historical
 enable-recursive-minibuffers t
 minibuffer-visible-completions t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 read-minibuffer-restore-windows nil)
(minibuffer-depth-indicate-mode 1)

;; savehist
(setq
 savehist-file (concat user-emacs-directory "savehist")
 history-length 30000
 history-delete-duplicates t
 savehist-save-minibuffer-history t)
(savehist-mode 1)

;; recentf
(add-hook 'after-init-hook 'recentf-mode)
(define-key global-map (kbd "C-x C-r") #'recentf-open)

;; saveplace
(save-place-mode 1)

;; isearch
(setq
 isearch-lazy-count t
 lazy-count-prefix-format "(%s/%s) "
 lazy-count-suffix-format nil)

;; ffap
(global-set-key [remap find-file] #'find-file-at-point)
(with-eval-after-load 'ffap
  (ffap-bindings))

;; etags
(defun user/visit-tags-table ()
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (let ((tags-file (locate-dominating-file buffer-file-name "TAGS")))
      (when tags-file
        (message "Loading tags file: %s" tags-file)
        (visit-tags-table tags-file t)))))
(add-hook 'find-file-hook 'user/visit-tags-table)
(setq tags-add-tables nil)
(setq etags-regen-tags-file
      (lambda (proj)
        (let* ((tags-directory (concat user-emacs-directory "tags"))
               (backup-directory-alist `(("." . ,tags-directory))))
          (make-backup-file-name-1 (concat proj "TAGS")))))
(when (fboundp 'etags-regen-mode)
  (add-hook 'prog-mode-hook 'etags-regen-mode))

;; xref
(when (> emacs-major-version 27)
  (setq
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-buffer
   xref-file-name-display 'project-relative))

;; icomplete
(setq
 icomplete-delay-completions-threshold 100
 icomplete-max-delay-chars 2
 icomplete-compute-delay 0.0
 icomplete-show-matches-on-no-input t
 icomplete-with-completion-tables t
 icomplete-in-buffer t
 icomplete-tidy-shadowed-file-names nil
 icomplete-hide-common-prefix nil)
(cond
 ((fboundp 'fido-vertical-mode) (fido-vertical-mode 1))
 ((fboundp 'icomplete-vertical-mode) (icomplete-vertical-mode 1))
 ((fboundp 'fido-mode) (fido-mode 1))
 (t (icomplete-mode 1)))
(define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<down>") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<up>") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-j") 'exit-minibuffer)
(define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit)
(define-key icomplete-minibuffer-map (kbd "TAB") 'icomplete-force-complete)
(unless (fboundp 'icomplete-force-complete)
  (defalias 'icomplete-force-complete 'minibuffer-force-complete))


;; window
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
(setq window-sides-vertical nil)

;; winner
(winner-mode)

;; windmove
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

;; tramp
(setq tramp-default-method "ssh")
(add-to-list
 'backup-directory-alist
 (cons tramp-file-name-regexp
       (concat user-emacs-directory "tramp-backups/")))

;; vc
(setq
 vc-ignore-dir-regexp
 (format "\\(%s\\)\\|\\(%s\\)"
         vc-ignore-dir-regexp
         tramp-file-name-regexp)
 vc-git-grep-template "git --no-pager grep --recurse-submodules -n --break <C> -e <R> -- <F>")
(defun user/vc-git-grep (regexp)
  (interactive
   (progn
     (grep-compute-defaults)
     (list (grep-read-regexp))))
  (let ((root (vc-git-root default-directory)))
    (vc-git-grep regexp "" (if root root default-directory))))
(global-set-key (kbd "C-x v f") 'user/vc-git-grep)

;; grep
(defun user/grep-mode-hook ()
  (switch-to-buffer-other-window "*grep*"))
(add-hook 'grep-mode-hook #'user/grep-mode-hook)

;; ediff
(setq
 ediff-split-window-function 'split-window-horizontally
 ediff-merge-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain)

;; gdb
(setq
 gdb-many-windows t
 gdb-show-main t)

;; warnings
(setq warning-suppress-types '((comp) (direnv)))

;; re-builder
(setq-default reb-re-syntax 'string)

;; sort
(defvar sort-fold-case)
(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

;; time-stamp
(setq-default
 time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S"
 time-stamp-time-zone t)
(add-hook 'before-save-hook 'time-stamp)

;; whitespace
(defun user/show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'user/show-trailing-whitespace)
(defun user/f90-highlight-long-lines ()
  (highlight-lines-matching-regexp ".\\{133\\}" 'whitespace-line))
(add-hook 'f90-mode-hook #'user/f90-highlight-long-lines)

;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;
;;;; PACKAGES ;;;;
;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;

;;;; This might be necessary on older Emacs
;; $ mkdir -pm0700 ~/.config/emacs/elpa/gnupg
;; $ gpg --keyserver keyserver.ubuntu.com --homedir ~/.config/emacs/elpa/gnupg --recv-keys 066DAFCB81E42C40
;; $ gpg --keyserver keyserver.ubuntu.com --homedir ~/.config/emacs/elpa/gnupg --recv-keys 645357D2883A0966


;; Always load the latest file (ignore outdated bytecode)
(setq load-prefer-newer t)

;; package archives
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("gnu" . 30)
                                   ("nongnu" . 20)
                                   ("melpa" . 10)
                                   ("melpa-stable" . 0)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-demand (daemonp))

;; project
(when (< emacs-major-version 28)
  (use-package project
    :ensure t)

  (use-package icomplete-vertical
    :ensure t
    :pin melpa-stable
    :config
    (icomplete-vertical-mode)))

(use-package diminish
  :ensure t)

(use-package evil
  :ensure t
  :commands evil-mode
  :config
  (setq
   evil-mode-line-format '(before . mode-line-front-space)
   evil-normal-state-tag   (propertize " NORMAL "   'face '((:weight bold :background "SpringGreen" )))
   evil-insert-state-tag   (propertize " INSERT "   'face '((:weight bold :background "DeepSkyBlue1")))
   evil-replace-state-tag  (propertize " REPLACE "  'face '((:weight bold :background "red3"        )))
   evil-visual-state-tag   (propertize " VISUAL "   'face '((:weight bold :background "DarkOrange"  )))
   evil-emacs-state-tag    (propertize " EMACS "    'face '((:weight bold :background "SkyBlue2"    )))
   evil-motion-state-tag   (propertize " MOTION "   'face '((:weight bold :background "plum3"       )))
   evil-operator-state-tag (propertize " OPERATOR " 'face '((:weight bold :background "sandy brown" )))))

(use-package magit
  :ensure t
  :defer 2
  :init
  (setq magit-define-global-key-bindings 'recommended))

(use-package diff-hl
  :ensure t
  :defer 2
  :after magit
  :config
  (defun user/diff-hl-mode-on-hook ()
    (unless (window-system)
      (if (fboundp 'diff-hl-margin-local-mode)
          (diff-hl-margin-local-mode)
        (diff-hl-margin-mode))))
  (add-hook 'diff-hl-mode-on-hook #'user/diff-hl-mode-on-hook)
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package xclip
  :ensure t
  :config
  (condition-case err
      (xclip-mode 1)
    (file-error (message "file-error: %S" err))))

;; AUCTeX
(defun user/align-environment ()
  "Apply align to the current environment only."
  (interactive)
  (save-excursion
    (LaTeX-mark-environment)
    (align (point) (mark))
    (pop-mark)))

(use-package tex-keywords
  :load-path "lisp"
  :defer t)

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq-default
   TeX-PDF-mode t
   TeX-quote-after-quote t
   TeX-parse-self t
   ;;TeX-auto-local nil
   ;;TeX-auto-save t
   TeX-command-Show "LaTeX"
   ;;TeX-view-program-selection '((output-pdf "Zathura"))
   TeX-source-correlate-start-server t
   TeX-debug-bad-boxes t
   TeX-debug-warnings t
   TeX-display-help 'expert
   ;;TeX-error-overview-open-after-TeX-run t
   TeX-parse-all-errors t
   LaTeX-reftex-cite-format-auto-activate nil))

(use-package latex
  :ensure auctex
  :defer t
  :config
  ;; TeX mode enhancements

  ;; Key bindings
  (defun user/LaTeX-mode-hook ()
    (defalias 'align-environment 'user/align-environment)
    (define-key LaTeX-mode-map "\C-ca" 'align-environment)
    (define-key LaTeX-mode-map "\C-xn" nil) ;; narrow-or-widen-dwim
    (define-key LaTeX-mode-map [down-mouse-3] 'imenu))
  (add-hook 'LaTeX-mode-hook 'user/LaTeX-mode-hook)

  ;; SyncTeX
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

  ;; LaTeX-math-mode
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  ;; Don't fontify math
  (with-eval-after-load 'font-latex
    (set-face-foreground 'font-latex-math-face nil))
  (setq font-latex-fontify-script nil)

  ;; Fontify primitives
  (require 'tex-keywords)
  (setq font-latex-match-function-keywords
        (mapcar #'(lambda (primitive) (list primitive ""))
                tex-keywords/primitives))

  ;; Override default indentation
  (setq LaTeX-indent-environment-list
   (quote
    (("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("filecontents" current-indentation)
     ("filecontents*" current-indentation)
     ("lstlisting" current-indentation))))
  (add-to-list 'LaTeX-verbatim-environments "lstlisting"))

(use-package context
  :ensure auctex
  :mode (("\\.mk\\(ii\\|iv\\|vi\\|xl\\|lx\\)\\'" . ConTeXt-mode)
         ("\\.mp\\(ii\\|iv\\|vi\\|xl\\|lx\\)\\'" . metapost-mode))
  :config
  (setq ConTeXt-Mark-version "IV")
  (defun user/ConTeXt-mode-hook ()
    (setq TeX-command-default "ConTeXt Full"
          TeX-command-Show "ConTeXt Full"))
  (add-hook 'ConTeXt-mode-hook #'user/ConTeXt-mode-hook))

(use-package cdlatex
  :ensure t
  :commands turn-on-cdlatex
  :init
  (setq
   cdlatex-takeover-parenthesis nil
   cdlatex-takeover-dollar nil
   cdlatex-takeover-subsuperscript nil)
  (add-hook 'cdlatex-tab-hook #'indent-for-tab-command)
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex))

(use-package reftex
  :defer t
  :init
  (setq-default
   reftex-plug-into-AUCTeX t
   reftex-label-alist '(AMSTeX)
   reftex-insert-label-flags '("s" t)
   reftex-cite-format 'default
   reftex-cite-key-separator ", ")
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  :config
  (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
  (add-hook 'reftex-mode-hook 'imenu-add-menubar-index))

(use-package bibref
  :load-path "lisp"
  :commands (bibref-from-aps
             bibref-from-doi
             bibref-from-arxiv))

;; bibtex
(use-package bibtex
  :bind (:map bibtex-mode-map
              ("C-c a" . bibref-from-aps)
              ("C-c d" . bibref-from-doi)
              ("C-c x" . bibref-from-arxiv)
              ("C-c v" . bibtex-validate)
              ("C-c s" . bibtex-sort-buffer)
              ([down-mouse-3] . imenu))
  :config
  (setq
   bibtex-align-at-equal-sign t
   bibtex-maintain-sorted-entries t))

;; direnv
(use-package direnv
  :if (executable-find "direnv")
  :ensure t
  :config
  (direnv-mode))

;; Language modes
(use-package cmake-mode
  :ensure t
  :defer t)
(use-package csv-mode
  :ensure t
  :defer t)
(use-package cuda-mode
  :ensure t
  :defer t)
(use-package dockerfile-mode
  :ensure t
  :defer t)
(use-package format-all
  :ensure t
  :commands format-all-mode
  ;; Only Fortran has a formatter configured below.
  :hook (f90-mode . format-all-mode)
  :config
  (define-format-all-formatter
   findent-octopus
   (:executable "findent-octopus")
   (:install)
   (:languages "Fortran Free Form")
   (:features)
   (:format (format-all--buffer-easy executable)))
  (setq-default format-all-formatters
                '(("Fortran Free Form" (findent-octopus "-i2" "-c2")))))
(use-package gnuplot
  :ensure t
  :mode ("\\.gnuplot\\'" . gnuplot-mode)
  :config (setq gnuplot-display-process nil))
(use-package go-mode
  :ensure t
  :defer t
  :config
  (defun user/go-mode-hook ()
    (add-hook 'before-save-hook #'gofmt-before-save nil t))
  (add-hook 'go-mode-hook #'user/go-mode-hook))
(use-package haskell-mode
  :ensure t
  :defer t)
(use-package json-mode
  :ensure t
  :defer t)
(use-package lua-mode
  :ensure t
  :mode "\\.Quanty\\'"
  :config (setq lua-indent-level 4))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode ("/README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))
(use-package nix-mode
  :ensure t
  :defer t)
(use-package rust-mode
  :ensure t
  :defer t)
(use-package terraform-mode
  :ensure t
  :defer t)
(use-package yaml-mode
  :ensure t
  :defer t)

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode))

;; Tree-sitter needs dynamic module loading
(when (and (functionp 'module-load) (bound-and-true-p module-file-suffix))
  (use-package tree-sitter
    :ensure t
    :config (global-tree-sitter-mode))
  (use-package tree-sitter-langs
    :ensure t
    :after tree-sitter
    :config (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

;; Editing plugins
(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t
  :defer 1
  :config
  (setq
   undo-tree-auto-save-history t
   undo-tree-history-directory-alist `(("\\`/dev/shm") ("." . ,(concat user-emacs-directory "undo")))
   undo-tree-enable-undo-in-region t)
  (global-undo-tree-mode 1))

;; Show suggestions for incomplete key chords
(use-package which-key
  :diminish which-key-mode
  :ensure t
  :defer 2
  :config (which-key-mode 1))

(use-package keycast
  :ensure t
  :commands (keycast-mode-line-mode keycast-header-line-mode
             keycast-tab-bar-mode keycast-log-mode))
