;;; early-init --- Early initialization file  -*- lexical-binding: t; -*-

;;; Code:

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq use-dialog-box nil)

(setq frame-resize-pixelwise t) 
(setq frame-inhibit-implied-resize t)

;; disable bars
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;; (when load-file-name
;;   (setq user-emacs-directory (file-name-directory load-file-name)))

;;; early-init.el ends here
