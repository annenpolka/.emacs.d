;;; init.el --- Annenpolka's emacs init.el
;;; Commentary:
;;; Code: Annenpolka

;; =======================================================================================
;; Startup/Package Management
;; =======================================================================================
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; package.el ------------------------------
(require 'package)
(add-to-list 'package-archives '("gnu-elpa-devel" . "https://elpa.gnu.org/devel/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; インストールする優先順位を指定
(setq package-archive-priorities
      '(("gnu-elpa-devel" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(setq package-install-upgrade-built-in t ; built-inパッケージも更新対象にする
      package-native-compile t           ; インストール時にnative compileする
      )

;; use-package and related ones -----------------------------
(use-package use-package
  :config
  (setq use-package-always-ensure t)
  (unless (package-installed-p 'vc-use-package)
    (package-vc-install "https://github.com/slotThe/vc-use-package"))
  (require 'vc-use-package)
  )
(use-package auto-package-update
  :config
  (setq auto-package-update-interval 1)
  (auto-package-update-maybe))
(use-package auto-compile
  :config
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1))

;; Windows hacks ---------------------------------------------
(when IS-WINDOWS
  ;; shift-jisよりcp932を優先させる
  (set-coding-system-priority 'utf-8
                              'euc-jp
                              'iso-2022-jp
                              'cp932)
  (setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos)))

(provide 'init)
;;; init.el ends herke
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((vc-use-package :vc-backend Git :url
		     "https://github.com/slotThe/vc-use-package"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
