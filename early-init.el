;;; early-init.el - Annenpolka's emacs early-init.el
;;; Commentary:
;;; Code: Annenpolka

;; Optimizations/QoL improvements
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

;; Disable GUI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here