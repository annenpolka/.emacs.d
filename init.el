;;; Annenpolka init.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code: Annenpolka

;; =======================================================================================
;; Startup/Package Management
;; =======================================================================================
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29
;; package.el ------------------------------
(require 'package)
(add-to-list 'package-archives '("gnu-elpa-devel" . "https://elpa.gnu.org/devel/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Highest number gets priority (what is not mentioned has priority 0)
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
  )
(use-package auto-package-update
  :config
  (setq auto-package-update-interval 7)
  (auto-package-update-maybe))
(use-package auto-compile
  :config
  (setq load-prefer-newer t)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1))

;; enacs built-in configs----------------------------------------------
(use-package emacs
  :ensure nil
  :bind* (("M-ESC ESC" . c/redraw-frame)
	  ("C-w" . 'backward-kill-word))
  :init
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (define-key key-translation-map [?\C-h] [?\C-?])
  (global-set-key (kbd "C-?") 'help-for-help)
  (define-key input-decode-map [?\C-i] [C-i])
  (show-paren-mode 1)
  (global-display-line-numbers-mode 1)
  (setq user-full-name "annenpolka"
        user-mail-address "lancelbb@gmail.com"
        user-login-name "annenpolka"
        default-directory "~/"
        backup-directory-alist '((".*" . "~/.backup"))
        ;; auth-sources '("~/.authinfo.gpg" "~/.netrc")
        create-lockfiles nil
        debug-on-error nil
        init-file-debug nil
        frame-resize-pixelwise t
        enable-recursive-minibuffers t
        history-length 1000
        history-delete-duplicates t
        scroll-preserve-screen-position t
        scroll-conservatively 100
        mouse-wheel-scroll-amount '(1 ((control) . 5))
        ring-bell-function 'ignore
        text-quoting-style 'straight
        truncate-lines t
        completion-cycle-threshold 3
        tab-always-indent 'complete
        default-tab-width 4
        scroll-bar-mode nil
        indent-tabs-mode nil
        vc-follow-symlinks t
        vc-handled-backends '(Git SVN)
        show-paren-style 'parenthesis
        show-paren-delay 0
        bookmark-watch-bookmark-file 'silent)
  )
;; config files to no-littering ------------------------------
(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "data/" user-emacs-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
;; Windows hacks ---------------------------------------------
(when IS-WINDOWS
  ;; shift-jisよりcp932を優先させる
  (set-coding-system-priority 'utf-8
                              'euc-jp
                              'iso-2022-jp
                              'cp932)
  (setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos)))
;; =======================================================================================
;; Linting/Formatting
;; =======================================================================================
;; formatter
(use-package apheleia
  :diminish apheleia-mode
  :config
  (apheleia-global-mode t))

;; =======================================================================================
;; Editor
;; =======================================================================================
;; double-key binding support-------------------------------------
(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.08
        key-chord-one-keys-delay 0.2)
  (key-chord-mode 1))
;; meow keymap-----------------------------------------------------
(use-package meow
  :init
  ;; command functions
  (defun meow-save-line nil
    (interactive)
    (meow-line 1)
    (call-interactively #'meow-save))
  (defun meow-insert-at-first-non-whitespace nil
    (interactive)
    (back-to-indentation)
    (meow-insert))
  (defun meow-insert-at-end-of-line nil
    (interactive)
    (move-end-of-line 1)
    (meow-insert))
  (defun meow-find-backward nil
    (interactive)
    (let ((current-prefix-arg -1))
      (call-interactively 'meow-find)))
  (defun meow-search-backward nil
    (interactive)
    (let ((current-prefix-arg -1))
      (call-interactively 'meow-search)))
  (defun meow-close-window-or-buffer ()
    (interactive)
    (if (one-window-p)
	(kill-this-buffer)
      (delete-window)))

  ;; setup keymap
  (defun meow-setup nil
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("C-u" . ccm-scroll-down)
     '("C-d" . ccm-scroll-up)
     '("C-o" . my/backward-forward-previous-location)
     '("<C-i>" . my/backward-forward-next-location)
     '("C-w" . meow-close-window-or-buffer)
     '("/" . consult-line)
     '("<escape>" . ignore))
    (meow-leader-define-key
     '("j" . "H-j")
     '("k" . "H-k")
     '("C-u" . "H-C-u")
     '("C-d" . "H-C-d")
     '("C-o" . "H-C-o")
     '("<C-i>" . "H-C-i")
     '("C-w" . "H-C-w")
     '("/" . "H-/")
     '("r e" . restart-emacs)
     '("w j" . move-or-create-window-below)
     '("w k" . move-or-create-window-above)
     '("w h" . move-or-create-window-left)
     '("w l" . move-or-create-window-right)
     '("w w" . window-swap-states)
     '("f" . "s-f")
     '("v" . my-git-actions/body)
     '("z g" . flyspell-correct-at-point)
     '("l" . "s-l")
     '("p" . "C-c M-p")
     '("K" . helpful-at-point)
     '("SPC" . consult-buffer)
     '("s f" . affe-find)
     '("s p" . affe-grep)
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '("=" . meow-query-replace)
     '("+" . meow-stellar-replace-regexp)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-insert-at-end-of-line)
     '("o" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     ;; '("d" . meow-delete)
     '("d" . meow-kill)
     '("D" . kill-line)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . avy-goto-char-timer)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-insert-at-first-non-whitespace)
     '("O" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("N" . meow-search-backward)
     '("(" . meow-block)
     '(")" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . repeat)
     ;; '("s" . meow-meow)
     '("s" . embrace-commander)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . undo-redo)
     '("C-r" . undo-redo)
     ;; '("v" . meow-visit)
     '("v" . er/expand-region)
     '("V" . er/contract-region)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("\"" . consult-yank-pop)
     '("/" . consult-line)
     '("C-u" . ccm-scroll-down)
     '("C-d" . ccm-scroll-up)
     '("C-o" . my/backward-forward-previous-location)
     '("<C-i>" . my/backward-forward-next-location)
     '("C-j" . move-or-create-window-below)
     '("C-k" . move-or-create-window-above)
     '("DEL" . move-or-create-window-left) ; C-h translated to DEL
     '("C-l" . move-or-create-window-right)
     '("C-f" . consult-line)
     ;; '("C-t" . burly-perspective-init-project-persp)
     '("C-s" . save-buffer)
     '("C-w" . meow-close-window-or-buffer)
     ;;  (cons "S-SPC" kurumi-utility-map)
     '("C-s" . save-buffer)
     '("<escape>" . ignore))
    ;; readline-style keymap in global map
    (bind-key "C-w" 'backward-kill-word)
    ;; remap universal-argument
    (bind-key "C-M-u" 'universal-argument)
    ;; insert state keymap
    (meow-define-keys
	'insert
      '("C-g" . meow-insert-exit))
    ;; key-chord shortcuts
    (key-chord-define meow-insert-state-keymap "jk" 'meow-insert-exit)
    (key-chord-define meow-normal-state-keymap "gd" 'xref-find-definitions))

  :hook
  (meow-insert-exit . (lambda nil (deactivate-input-method)))
  (meow-insert-exit . (lambda nil (call-interactively 'corfu-quit)))
  (meow-insert-exit . (lambda nil (key-chord-mode 1)))
  :config
  (setq meow-use-clipboard t
	meow-keypad-self-insert-undefined nil
	meow-mode-state-list '((helpful-mode . normal)
			       (help-mode . normal)
			       (Man-mode . normal)
			       (eww-mode . normal)
			       (devdocs-mode . normal)
			       (howm-menu-mode . motion)
			       (howm-view-summary-mode . motion)
			       (vterm-mode . insert)
			       (eshell-mode . insert))
	meow--kbd-forward-char "<right>"
	;; (meow--kbd-forward-line . "<down>")
	;; (meow--kbd-backward-line . "<up>")
	meow--kbd-delete-char "<deletechar>"
	meow--kbd-kill-region "S-<delete>"
	meow--kbd-kill-line "<deleteline>"
	meow-selection-command-fallback '((meow-reverse . back-to-indentation)
                                          (meow-change . meow-change-char)
                                          (meow-save . meow-save-line)
                                          (meow-kill . meow-delete)
                                          ;;  (meow-kill . meow-kill-whole-line)
                                          (meow-pop-selection . meow-pop-grab)
                                          (meow-beacon-change . meow-beacon-change-char)
                                          (meow-cancel . keyboard-quit)
                                          (meow-delete . meow-C-d))
	meow-char-thing-table '((?r . round)
				(?\( . round)
				(?b . anyblock) ;; `b' for bracket
				(?c . curly)
				(?s . string)
				(?\" . string)
				(?e . symbol)
				(?w . window)
				(?B . buffer)
				(?g . buffer)
				(?p . paragraph)
				(?\[ . square)
				(?l . line)
				(?d . defun)
				(?. . sentence)))
  (meow-setup)
  (meow-global-mode 1))


;; =======================================================================================
;; git
;; =======================================================================================
(use-package magit
  :ensure t
  :bind (:map magit-status-mode-map
	      ("p" . magit-pull)
	      ("x" . magit-delete-thing)))
(use-package forge
  :after magit
  :disabled
  ;; :if (unless IS-WINDOWS) ; forge doesn't work on windows
  :custom
  (bug-reference-mode 0)
  (forge-add-default-bindings t))

(use-package git-gutter
  :diminish git-gutter-mode
  :custom
  (git-gutter:ask-p nil)
  :init
  (global-git-gutter-mode))

(use-package git-auto-commit-mode
  :hook
  (howm-mode . git-auto-commit-mode)
  :custom
  (gac-silent-message-p t)
  (gac-automatically-push-p t))
(provide 'init)
;;; init.el ends herke
