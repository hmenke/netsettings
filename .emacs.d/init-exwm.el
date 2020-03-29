(use-package exwm
  :ensure t
  :config
  ; default config
  (require 'exwm-config)
  (exwm-config-default)

  ; systray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ; set some variables
  (setq mouse-autoselect-window t
        focus-follows-mouse t
        exwm-manage-configurations '((t char-mode t)))

  ; keybindings
  (global-set-key (kbd "s-k") 'exwm-workspace-delete)
  (global-set-key (kbd "s-w") 'exwm-workspace-swap)
  (global-set-key (kbd "s-k") 'exwm-input-release-keyboard)
  )
