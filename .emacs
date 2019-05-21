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

; Show matching parentheses
(show-paren-mode 1)

; mouse integration
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

; package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

; install not-installed packages
(defun package-dl (p)
  (unless (package-installed-p p)
    (progn
      (package-refresh-contents)
      (package-install p))))

; Vim bindings
(package-dl 'evil)
(require 'evil)
(evil-mode 1)

; Language modes
(package-dl 'auctex)
(package-dl 'clang-format)
(package-dl 'cmake-mode)
(package-dl 'cuda-mode)
(package-dl 'cython-mode)
(package-dl 'd-mode)
(package-dl 'gnuplot-mode)
(package-dl 'haskell-mode)
(package-dl 'julia-mode)
(package-dl 'lua-mode)
(package-dl 'modern-cpp-font-lock)
(package-dl 'rust-mode)

; Theme
(package-dl 'gruvbox-theme)
(load-theme 'gruvbox t)

; c++ mode enhancements
(setq c-default-style "linux" c-basic-offset 4)
(c-set-offset 'innamespace 0)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
(defun enable-clang-format ()
  (require 'clang-format)
  (global-set-key [C-M-tab] 'clang-format-region))
(add-hook 'c-mode-hook 'enable-clang-format)
(add-hook 'c++-mode-hook 'enable-clang-format)

; TeX mode enhancements
(setq TeX-PDF-mode t)
(setq TeX-quote-after-quote t)
(setq-default TeX-engine 'luatex)
(setq TeX-command-Show "LaTeX")
(setq TeX-view-program-selection '((output-pdf "XDG")))
(setq TeX-view-program-list '(("XDG" "xdg-open %o")))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)
(setq reftex-label-alist '(AMSTeX))
(eval-after-load "font-latex"
  '(set-face-foreground 'font-latex-math-face nil))
(setq font-latex-fontify-script nil)

; ConTeXt mode
(add-to-list 'auto-mode-alist '("\\.mkii\\'" . ConTeXt-mode))
(add-to-list 'auto-mode-alist '("\\.mkiv\\'" . ConTeXt-mode))
(add-to-list 'auto-mode-alist '("\\.mkvi\\'" . ConTeXt-mode))
(add-to-list 'auto-mode-alist '("\\.mpii\\'" . metapost-mode))
(add-to-list 'auto-mode-alist '("\\.mpiv\\'" . metapost-mode))
(add-to-list 'auto-mode-alist '("\\.mpvi\\'" . metapost-mode))
(setq ConTeXt-Mark-version "IV")
(with-eval-after-load "context"
  (add-to-list 'TeX-file-extensions "mkiv" t)
  (add-to-list 'TeX-file-extensions "mkvi" t))
(add-hook 'ConTeXt-mode-hook
          (lambda()
            (setq TeX-command-default "ConTeXt Full")
            (setq TeX-command-Show "ConTeXt Full")))

; Lua mode
(setq lua-indent-level 4)

; Dired enhancements
(require 'dired-x)
(setq dired-guess-shell-alist-user (list (list "\\.pdf$" "xdg-open")))
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

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

(dired ".")
