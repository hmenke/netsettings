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
(setq line-number-mode t)
(setq column-number-mode t)
(setq x-select-enable-clipboard t)
(setq backup-by-copying t)
(setq-default indent-tabs-mode nil)
(setq visual-line-fringe-indicators
      '(left-curly-arrow right-curly-arrow))
(setq c-default-style "linux" c-basic-offset 4)
(c-set-offset 'innamespace 0)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
(require 'whitespace)


; DocView properties
(setq doc-view-continuous t)
(setq doc-view-resolution 200)


; Show matching parentheses
(show-paren-mode 1)


; Replacement for multiple-cursors
(cua-selection-mode t)
(global-set-key (kbd "C-v") 'cua-set-rectangle-mark)


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

(package-dl 'auctex)
; (require 'auctex) ; apparently not needed

(package-dl 'evil)
(require 'evil)

(package-dl 'cmake-mode)
(require 'cmake-mode)

(package-dl 'cuda-mode)
(require 'cuda-mode)

(package-dl 'cython-mode)
(require 'cython-mode)

(package-dl 'd-mode)
(require 'd-mode)

(package-dl 'gnuplot-mode)
(require 'gnuplot-mode)

(package-dl 'gruvbox-theme)
(load-theme 'gruvbox t)

(package-dl 'haskell-mode)
(require 'haskell-mode)

(package-dl 'julia-mode)
(require 'julia-mode)

(package-dl 'lua-mode)
(require 'lua-mode)

(package-dl 'modern-cpp-font-lock)

(package-dl 'rust-mode)
(require 'rust-mode)


; TeX mode enhancements
(setq TeX-PDF-mode t)
(setq-default TeX-engine 'luatex)
(setq TeX-command-Show "LaTeX")
(setq TeX-view-program-selection '((output-pdf "XDG")))
(setq TeX-view-program-list '(("XDG" "xdg-open %o")))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)
(setq reftex-label-alist '(AMSTeX))


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

; Dired enhancements
(setq dired-listing-switches
      "--group-directories-first -l --hide=*~")
(add-hook 'dired-mode-hook
  (lambda ()
    (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)))

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
