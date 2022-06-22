;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Author: annenpolka <lancelbb@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

; ╭──────────────────────────────────────────────────────────╮
; │                           leaf                           │
; ╰──────────────────────────────────────────────────────────╯
;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; leaf plugins
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

;; explain macro by step
(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

; ╭──────────────────────────────────────────────────────────╮
; │                       basic, chore                       │
; ╰──────────────────────────────────────────────────────────╯
;; supress leaf's :custom auto append to custom.el
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

;; set builtin configs via leaf
(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((user-full-name . "annenpolka")
            (user-mail-address . "lancelbb@gmail.com")
            (user-login-name . "annenpolka")
            (create-lockfiles . nil)
            (debug-on-error . t)
            (init-file-debug . t)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 1000)
            (history-delete-duplicates . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (text-quoting-style . 'straight)
            (truncate-lines . t)
            ;; (use-dialog-box . nil)
            ;; (use-file-dialog . nil)
            ;; (menu-bar-mode . t)
            ;; (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

; ╭──────────────────────────────────────────────────────────╮
; │                          themes                          │
; ╰──────────────────────────────────────────────────────────╯
;; font config
(defvar my/font-family "Iosevka Term")
(defvar my/font-size
  (let ((size-by-hostname
         '(("DESKTOP-B6V868U" . 12.5))))
    (or (cdr (assoc (system-name) size-by-hostname))
        13.5)))

(when window-system
  ;; http://d.hatena.ne.jp/kitokitoki/20110502/p2
  (let ((fontset (format "%s-%.1f" my/font-family my/font-size)))
    (add-to-list 'default-frame-alist `(font . ,fontset)))
  (add-to-list 'default-frame-alist `(cursor-type . (hbar . ,(1+ (ceiling (/ my/font-size 2)))))))

;; modus theme
(leaf modus-themes
  :custom
  (modus-themes-completions . 'moderate)
  (modus-themes-fringes . 'subtle)
  (modus-themes-italic-constructs . t)
  (modus-themes-bold-constructs . nil)
  (modus-themes-hl-line . '(intense underline))
  (modus-themes-region . '(bg-only no-extend))
  (modus-themes-scale-headings . t)
  (modus-themes-prompts . '(background bold gray intense italic))
  (modus-themes-syntax . '(faint alt-syntax green-strings))
  :config
   (load-theme 'modus-vivendi))
; ╭──────────────────────────────────────────────────────────╮
; │                     helper interface                     │
; ╰──────────────────────────────────────────────────────────╯
(leaf which-key
  :doc "which-key in emacs"
  :ensure t
  :blackout which-key-mode
  :custom
  (which-key-idle-delay . 1.0)
  :init
  (which-key-setup-minibuffer)
  (which-key-mode t))

(leaf hl-todo
  :doc "TODO keywords highlighting"
  :ensure t
  :global-minor-mode global-hl-todo-mode)
; ╭──────────────────────────────────────────────────────────╮
; │                      editing modal                       │
; ╰──────────────────────────────────────────────────────────╯
(leaf evil
  :doc "Extensible vi layer for Emacs."
  :require evil windmove
  :ensure t
  :pre-setq
  ;; for evil-collection
  (evil-want-keybinding . nil)
  :custom
  ;; <C-u> to scroll (replace universal-argument)
  (evil-want-C-u-scroll . t)
  ;; serach module
  (evil-search-module 'evil-search)
  :config
  ;; leader-key
  (evil-set-leader 'normal (kbd "<SPC>"))
  ;; activate evil
  (evil-mode 1)
  (turn-on-evil-mode)
  :bind
  ((:evil-normal-state-map
     ;; move to window by windmove
    ("<leader>h" . windmove-left)
    ("<leader>j" . windmove-down)
    ("<leader>k" . windmove-up)
    ("<leader>l" . windmove-right))))

(leaf evil-collection
  :after evil
  :ensure t
  :config
    (evil-collection-init '(magit)))
; ╭──────────────────────────────────────────────────────────╮
; │                           Git                            │
; ╰──────────────────────────────────────────────────────────╯
(leaf magit
  :doc "great git client"
  :ensure t)

(leaf forge
  :doc "remote repo control with magit"
  :ensure t
  :after magit
  :custom ((bug-reference-mode . 0)))

(leaf magit-todos
  :doc "manage TODO keywords with magit"
  :ensure t
  :after magit
  :hook (magit-mode-hook . magit-todos-mode))

; ╭──────────────────────────────────────────────────────────╮
; │                        completion                        │
; ╰──────────────────────────────────────────────────────────╯
(leaf vertico
  :ensure t
  :init (vertico-mode)
  :custom ((vertico-cycle . t))
  :bind ((:minibuffer-local-map
          ("C-j" . next-line-or-history-element)
          ("C-k" . previous-line-or-history-element))))

;;; orderless
(leaf orderless
  :ensure t
  :custom ((completion-styles . '(orderless))
         (orderless-smart-case . t)
         (orderless-matching-styles . '(orderless-prefixes
                                        orderless-initialism
                                        orderless-regexp))))

(leaf savehist
  :ensure t
  :init (savehist-mode))

;;; marginalia
(leaf marginalia
  :ensure t
  :init (marginalia-mode))

;;; embark
(leaf embark
  :ensure t)

;;; consult
(leaf consult
  :ensure t
  :setq ((completion-in-region-function . (lambda (&rest args)
                                            (apply (if vertico-mode
                                                       #'consult-completion-in-region
                                                     #'completion--in-region)
                                                   args)))
         (consult-buffer-sources . '(consult--source-hidden-buffer
                                     consult--source-buffer)))
  )
; ╭──────────────────────────────────────────────────────────╮
; │                       input method                       │
; ╰──────────────────────────────────────────────────────────╯
;; mozc ime
(leaf mozc
  ;; :depends emacs-mozc
  :bind* (("<zenkaku-hankaku>" . toggle-input-method))
  :custom ((default-input-method . "japanese-mozc")
	   (mozc-candidate-style quote overlay))
  :config
  (leaf-handler-package mozc mozc nil))

; ╭──────────────────────────────────────────────────────────╮
; │                       boilerplate                        │
; ╰──────────────────────────────────────────────────────────╯
(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(hydra leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
