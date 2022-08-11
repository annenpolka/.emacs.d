;;; early-init --- Early initialization file  -*- lexical-binding: t; -*-

;;; Code:

(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.5)

;; Prioritize old byte-compiled source files over newer sources. It saves us a
;; little IO time to skip all the mtime checks on each lookup.
(setq load-prefer-newer nil)


(let ((old-file-name-handler-alist file-name-handler-alist))
  ;; `file-name-handler-alist' is consulted on each `require', `load' and
  ;; various path/io functions. You get a minor speed up by unsetting this.
  ;; Some warning, however: this could cause problems on builds of Emacs where
  ;; its site lisp files aren't byte-compiled and we're forced to load the
  ;; *.el.gz files (e.g. on Alpine).
  (setq-default file-name-handler-alist nil)
  ;; ...but restore `file-name-handler-alist' later, because it is needed for
  ;; handling encrypted or compressed files, among other things.
  (defun doom-reset-file-handler-alist-h ()
    (setq file-name-handler-alist
          ;; Merge instead of overwrite because there may have bene changes to
          ;; `file-name-handler-alist' since startup we want to preserve.
          (delete-dups (append file-name-handler-alist
                               old-file-name-handler-alist))))
  (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h 101))

;; Premature redisplays can substantially affect startup times and produce
;; ugly flashes of unstyled Emacs.
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq use-dialog-box nil)

(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(setq inhibit-splash-screen t)

;; disable bars
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;; (when load-file-name
;;   (setq user-emacs-directory (file-name-directory load-file-name)))

(setq package-enable-at-startup nil)
(setq native-comp-async-report-warnings-errors nil)
;; suppress cl deprecation warnings
(setq byte-compile-warnings '(not cl-functions obsolete))
;; suppress ad-redefinition warnings
(setq ad-redefinition-action 'accept)

;; disable straight-process-buffer
(defvar straight-process-buffer)
(setq-default straight-process-buffer " *straight-process*")

;;; early-init.el ends here
