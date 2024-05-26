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
;; package.el -----------------------------------------------------------------------------
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

;; ローカルのパッケージパスを通す ----------------------------------------------------------
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("modules"))

(use-package prot-common
  :ensure nil
  :functions (prot-common-truncate-lines-silently)
  :hook ((fundamental-mode text-mode prog-mode) . prot-common-truncate-lines-silently)
  :config
  ;; NEVER tell me which key can call a command that I specifically
  ;; invoked with M-x: I have a good reason to use it that way.
  (advice-add #'execute-extended-command--describe-binding-msg :override #'prot-common-ignore))

;; use-package and related ones ---------------------------------------------------------
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

;; emacs built-in configs ---------------------------------------------------------------
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
  (global-so-long-mode +1)
  (electric-pair-mode 1)
  (electric-quote-mode 1)
  (electric-indent-mode 1)
  (delete-selection-mode 1)
  (setq-default indent-tabs-mode nil)
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
;; config files to no-littering ----------------------------------------------------------
(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "data/" user-emacs-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
;; hide minor-modes -----------------------------------------------------------------------
(use-package diminish)
;; romaji library -------------------------------------------------------------------------
(use-package migemo
  :if (executable-find "cmigemo")
  :init
  (let ((scoop-path (and IS-WINDOWS
                         (executable-find "scoop")
                         (concat (getenv "USERPROFILE") "/scoop/apps/cmigemo/current/cmigemo-default-win64/dict/utf-8/migemo-dict")))
        (default-path (cond (IS-WINDOWS
                             (concat (file-name-directory (executable-find "cmigemo")) "dict/utf-8/migemo-dict"))
                            (IS-LINUX
                             "/usr/share/cmigemo/utf-8/migemo-dict"))))
    (setq migemo-dictionary (or scoop-path default-path)))
  :config
  (when (file-exists-p migemo-dictionary)
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "-e"))
    (setq migemo-coding-system 'utf-8-unix)
    (migemo-init)))
;; OS hacks -------------------------------------------------------------------------
(when IS-WINDOWS
  ;; shift-jisよりcp932を優先させる
  (set-coding-system-priority 'utf-8
                              'euc-jp
                              'iso-2022-jp
                              'cp932)
  (setq-default default-process-coding-system '(utf-8-unix . japanese-cp932-dos)))
(when IS-WINDOWS
  (setq w32-use-native-image-API t))
(unless IS-MAC
  (setq command-line-ns-option-alist nil))
(unless IS-LINUX
  (setq command-line-x-option-alist nil))

;; `window', `display-buffer-alist', and related ----------------------------------------
(use-package prot-window
  :ensure nil
  :demand t
  :config
  (setq display-buffer-alist
        `(;; no window
          ("\\`\\*Async Shell Command\\*\\'"
           (display-buffer-no-window))
          ("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
           (display-buffer-no-window)
           (allow-no-window . t))
          ;; bottom side window
          ("\\*Org \\(Select\\|Note\\)\\*" ; the `org-capture' key selection and `org-add-log-note'
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((mode-line-format . none))))
          ;; bottom buffer (NOT side window)
          ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                  (derived-mode . flymake-project-diagnostics-mode)
                  (derived-mode . messages-buffer-mode)
                  (derived-mode . backtrace-mode)))
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . 0.3)
           (dedicated . t)
           (preserve-size . (t . t)))
          ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom))
          ;; below current window
          ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
           (display-buffer-reuse-mode-window display-buffer-below-selected))
          ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.1)
           (dedicated . t)
           (preserve-size . (t . t)))
          ((derived-mode . reb-mode) ; M-x re-builder
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 4) ; note this is literal lines, not relative
           (dedicated . t)
           (preserve-size . (t . t)))
          ((or . ((derived-mode . occur-mode)
                  (derived-mode . grep-mode)
                  (derived-mode . Buffer-menu-mode)
                  (derived-mode . log-view-mode)
                  (derived-mode . help-mode) ; See the hooks for `visual-line-mode'
                  "\\*\\(|Buffer List\\|Occur\\|vc-change-log\\|eldoc.*\\).*"
                  prot-window-shell-or-term-p
                  ;; ,world-clock-buffer-name
                  ))
           (prot-window-display-buffer-below-or-pop)
           (body-function . prot-window-select-fit-size))
          ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (dedicated . t)
           (window-height . fit-window-to-buffer))
          ;; NOTE 2022-09-10: The following is for `ispell-word', though
          ;; it only works because I override `ispell-display-buffer'
          ;; with `prot-spell-ispell-display-buffer' and change the
          ;; value of `ispell-choices-buffer'.
          ("\\*ispell-top-choices\\*.*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer))
          ;; same window

          ;; NOTE 2023-02-17: `man' does not fully obey the
          ;; `display-buffer-alist'.  It works for new frames and for
          ;; `display-buffer-below-selected', but otherwise is
          ;; unpredictable.  See `Man-notify-method'.
          ((or . ((derived-mode . Man-mode)
                  (derived-mode . woman-mode)
                  "\\*\\(Man\\|woman\\).*"))
           (display-buffer-same-window)))))

(use-package prot-window
  :ensure nil
  :demand t
  :config
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq split-height-threshold 80)
  (setq split-width-threshold 125)
  (setq window-min-height 3)
  (setq window-min-width 30))

(use-package prot-window
  :ensure nil
  :demand t
  :hook
  ((epa-info-mode help-mode custom-mode) . visual-line-mode))

(use-package prot-window
  :ensure nil
  :demand t
  :hook
  ((world-clock-mode calendar-mode) . prot-common-truncate-lines-silently))

(provide 'prot-emacs-window)
;; =======================================================================================
;; IME
;; =======================================================================================
;; IME Patch for Windows -----------------------------------------------------------------
(use-package tr-ime
  :if (when IS-WINDOWS)
  :hook
  (w32-ime-on-hook . (lambda() (key-chord-mode 1)))
  (w32-ime-off-hook . (lambda() (key-chord-mode 1)))
  :config
  (tr-ime-advanced-install 'no-confirm)
  (setq default-input-method "W32-IME")
  (modify-all-frames-parameters '((ime-font . "Migu 1P-12")))
  (w32-ime-initialize)
  ;; IME 制御（yes/no などの入力の時に IME を off にする）
  (wrap-function-to-control-ime 'universal-argument t nil)
  (wrap-function-to-control-ime 'read-string nil nil)
  (wrap-function-to-control-ime 'read-char nil nil)
  (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
  (wrap-function-to-control-ime 'y-or-n-p nil nil)
  (wrap-function-to-control-ime 'yes-or-no-p nil nil)
  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
  (wrap-function-to-control-ime 'register-read-with-preview nil nil))

;; japanese input method for linux -------------------------------------------------------
(use-package mozc
  :if (when IS-LINUX)
  :demand t
  :bind*
  (("<zenkaku-hankaku>" . toggle-input-method)
   ("<eisu-toggle>" . toggle-input-method))
  :hook
  (input-method-deactivate-hook . (lambda() (key-chord-mode 1)))
  :config
  (setq default-input-method "japanese-mozc-im")
  ;; (setq mozc-candidate-style 'popup)
  )
(use-package mozc-im
  :after mozc)
(use-package mozc-popup
  :after mozc)
;; (use-package mozc-cand-posframe
;;   :after mozc
;;   :config
;;    (setq mozc-candidate-style 'posframe))
(use-package mozc-temp
  :bind*
  ("C-j" . mozc-temp-convert-dwim)
  :after mozc)
;; =======================================================================================
;; files
;; =======================================================================================
(use-package recentf
  :ensure nil
  :init
  (setq recentf-save-file "~/.emacs.d/recentf"
        recentf-max-saved-items 2000
        recentf-auto-cleanup 'never
	recentf-keep          '(file-remote-p file-readable-p))
  :config
  (setq recentf-exclude '("recentf"
                          "COMMIT_EDITMSG"
                          "bookmarks"
                          "\\.gitignore"
                          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
                          "^/tmp/"
                          "^/scp:"
                          "~/.emacs.d/straight/.*"
                          "~/howm/.*"
                          (lambda (file) (file-in-directory-p file package-user-dir))))
  (recentf-mode 1))
;; save final place on each file
(use-package saveplace
  :ensure nil
  :config
  (save-place-mode 1))
;; "revert buffers when files on disk change"
(use-package autorevert
  :ensure nil
  :custom ((auto-revert-interval 1))
  :init
  (global-auto-revert-mode 1))

;; =======================================================================================
;; minibuffer
;; =======================================================================================
;; vertical completion minibuffer ui
(use-package vertico
  :init
  (setq vertico-cycle t)
  :config
  (vertico-mode t)
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :bind*
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim))        ;; good alternative: M-.
  :bind
  ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; complition-styles
(use-package orderless
  :config
  (icomplete-mode)
  (defun orderless-migemo (component)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))

  (orderless-define-completion-style orderless-default-style
    (orderless-matching-styles '(orderless-initialism
				 orderless-literal
                                 orderless-regexp)))

  (orderless-define-completion-style orderless-migemo-style
    (orderless-matching-styles '(orderless-initialism
				 orderless-literal
                                 orderless-regexp
                                 orderless-migemo)))

  (orderless-define-completion-style orderless-fuzzy-style
    (orderless-matching-styles '(orderless-initialism
				 orderless-literal
				 orderless-flex
                                 orderless-regexp
                                 orderless-migemo)))
  (setq completion-category-overrides
        '((command (styles orderless-default-style))
          (file (styles orderless-migemo-style))
          (buffer (styles orderless-migemo-style))
          (symbol (styles orderless-default-style))
          (consult-location (styles orderless-migemo-style)) ; category `consult-location' は `consult-line' などに使われる
          (consult-multi (styles orderless-migemo-style)) ; category `consult-multi' は `consult-buffer' などに使われる
          (org-roam-node (styles orderless-migemo-style)) ; category `org-roam-node' は `org-roam-node-find' で使われる
          (howm-fuzzy (styles orderless-fuzzy-style))
          (unicode-name (styles orderless-migemo-style))
          (variable (styles orderless-default-style))))

  ;;
  (add-to-list 'marginalia-prompt-categories '("\\<Keyword\\>" . howm-fuzzy))
  ;; (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-migemo))
  :custom
  (completion-styles '(orderless basic))
  )
;; =======================================================================================
;; Linting/Formatting
;; =======================================================================================
;; formatter -----------------------------------------------------------------------------
(use-package apheleia
  :diminish apheleia-mode
  :config
  (apheleia-global-mode t))

;; =======================================================================================
;; Visual/Interfaces
;; =======================================================================================
;; focus.nvim-like move-or-create-window -------------------------------------------------
(use-package emacs
  :ensure nil
  :init
  (defun move-or-create-window-above nil
    "Move to the window above the current one, or create a new split if none exists."
    (interactive)
    (unless (window-in-direction 'above)
      (split-window-below))
    (windmove-up))

  (defun move-or-create-window-below nil
    "Move to the window below the current one, or create a new split if none exists."
    (interactive)
    (unless (window-in-direction 'below)
      (split-window-vertically))
    (windmove-down))

  (defun move-or-create-window-left nil
    "Move to the window to the left of the current one, or create a new split if none exists."
    (interactive)
    (unless (window-in-direction 'left)
      (split-window-right))
    (windmove-left))

  (defun move-or-create-window-right nil
    "Move to the window to the right of the current one, or create a new split if none exists."
    (interactive)
    (unless (window-in-direction 'right)
      (split-window-horizontally))
    (windmove-right)))

;; icons -----------------------------------------------------------------------------------
(use-package all-the-icons
  :if (display-graphic-p))
;; themes ----------------------------------------------------------------------------------
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;; search/narrow ---------------------------------------------------------------------------
(use-package consult
  :init
  ;; *で始まるバッファ名を候補から削除 -> たとえば*scratch*に変えられない副作用
  ;; (setq ido-ignore-buffers (append '("\\`\\*") ido-ignore-buffers))
  :custom
  (consult-buffer-sources
   '
   (
    consult--source-buffer
    consult--source-hidden-buffer
    consult--source-recent-file))
  )
;; dir extension
(use-package consult-dir
  :after consult
  :bind
  (("C-x C-d" . consult-dir)
   (:map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file))))

;; font configuration ----------------------------------------------------------------------
(use-package fontaine
  :init
  (setq fontaine-presets
        '((tiny
           :default-height 70)
          (small
           :default-height 90)
          (regular
           :default-height 120)
          (medium
           :default-height 130)
          (large
           :default-weight semilight
           :default-height 140
           :bold-weight extrabold)
          (presentation
           :default-weight semilight
           :default-height 170
           :bold-weight extrabold)
          (jumbo
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (t ; shared fallback properties
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "PlemolJP Console NF"
           :default-weight regular
           :default-height 120
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "PlemolJP Console NF"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))
  ;; Recover last preset or fall back to desired style from
  ;; `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; set japanese font manually
  (defun set-japanese-font-face nil
    (set-fontset-font t 'japanese-jisx0208 (font-spec :family "migu 1M")))
  ;; (set-japanese-font-face)
  ;; :hook
  ;; hook for daemon mode
  ;; (server-after-make-frame-hook . set-japanese-font-face)
  :bind
  ("C-c F" . fontaine-set-preset))

;; rainbows ------------------------------------------------------------------------------
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-identifiers
  :hook
  (prog-mode . rainbow-identifiers-mode))
;; =======================================================================================
;; suggestion/autocompletion
;; =======================================================================================
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt) ;; Always preselect the prompt
  (corfu-auto t)
  (corfu-auto-delay 0.01)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-on-exact-match nil)
  (tab-always-indent 'complete)
  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :hook
  (prog-mode . corfu-mode))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :hook
  (((prog-mode
     text-mode) . my/set-super-capf))
  :init
  (defun my/set-super-capf (&optional arg)
    (setq-local completion-at-point-functions
                (list (cape-capf-noninterruptible
		       (cape-capf-buster
                        (cape-capf-properties
                         (cape-capf-super
                          (if arg
			      arg
                            (car completion-at-point-functions))
                          ;; #'tempel-complete
                          ;; #'tabnine-completion-at-point
                          #'cape-dabbrev
                          #'cape-file)
                         :sort t
                         :exclusive 'no))))))
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )
(use-package prescient
  :config
  (setq prescient-aggressive-file-save t)
  (prescient-persist-mode +1))

(use-package corfu-prescient
  :after corfu
  :config
  (with-eval-after-load 'orderless
    (setq corfu-prescient-enable-filtering nil))
  (corfu-prescient-mode +1))
;; =======================================================================================
;; Editor
;; =======================================================================================
;; double-key binding support ------------------------------------------------------------
(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.08
        key-chord-one-keys-delay 0.2)
  (key-chord-mode 1))
;; scroll with cursor --------------------------------------------------------------------
(use-package centered-cursor-mode
  :diminish centered-cursor-mode
  :config
  (global-centered-cursor-mode t)
  (setq ccm-step-size 2
        ccm-recenter-at-end-of-file t)
  ;; exclude on vterm
  (add-to-list 'ccm-ignored-commands 'vterm--self-insert))

;; better undo/redo ----------------------------------------------------------------------
(use-package undo-fu
  :ensure t
  :bind
  ([remap undo] . undo-fu-only-undo)
  ([remap redo] . undo-fu-only-redo))

;; vim-like historical locate navigation -------------------------------------------------
(use-package backward-forward
  :ensure t
  :init
  ;; reference: https://emacs-china.org/t/emacs/19171/17
  (defun my/backward-forward-previous-location ()
    "A `backward-forward-previous-location' wrap for skip invalid locations."
    (interactive)
    (let ((purge (< backward-forward-mark-ring-traversal-position (1- (length backward-forward-mark-ring))))
          (recent (point-marker)))
      (backward-forward-previous-location)
      (when (and (equal recent (point-marker)) purge)
        (my/backward-forward-previous-location))))

  (defun my/backward-forward-next-location ()
    "A `backward-forward-next-location' wrap for skip invalid locations."
    (interactive)
    (let ((purge (> backward-forward-mark-ring-traversal-position 0))
          (recent (point-marker)))
      (backward-forward-next-location)
      (when (and (equal recent (point-marker)) purge)
        (my/backward-forward-next-location))))
  :config
  (setq backward-forward-mark-ring-max 100)
  (backward-forward-mode 1))

;; blockman-like block highlighting ----------------------------------------------------
(use-package hl-block-mode
  :commands (hl-block-mode)
  :config
  (setq hl-block-color-tint "#010101"
	hl-block-delay 0.1
        hl-block-style 'color-tint
        hl-block-multi-line t
	hl-block-single-level nil)
  :hook ((prog-mode) . hl-block-mode))

;; smart soft deletion for parens ------------------------------------------------------
;; Use puni-mode globally and disable it for term-mode.
(use-package puni
  :defer t
  :bind
  ([remap kill-line] . puni-kill-line)
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

;; meow keymap ---------------------------------------------------------------------------
(use-package meow
  :init
  ;; --- dependencies ---
  (use-package expand-region)
  (use-package embrace)

  ;; --- command functions ---
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

  ;; --- setup/keymap ---
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
;; notetaking/writing
;; =======================================================================================
;; Markdown major mode -------------------------------------------------------------------
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :hook ((markdown-mode . turn-off-auto-fill)
	 (markdown-mode . visual-line-mode))
  :init
  (use-package word-wrap-mode
    :ensure nil
    :hook (visual-line-mode . word-wrap-whitespace-mode)
    :config
    (add-to-list 'word-wrap-whitespace-characters ?\]))

  (use-package visual-fill-column
    :hook (visual-line-mode . visual-fill-column-mode)
    :init
    (setq visual-line-fringe-indicators '(left-curly-arrow nil))
    :config
    (setq visual-fill-column-width 120))

  (use-package adaptive-wrap
    :hook (visual-line-mode . adaptive-wrap-prefix-mode))

  (setq markdown-command "pandoc --from=markdown --to=html5"
        markdown-fontify-code-blocks-natively t
        markdown-header-scaling t
        markdown-hide-url t
        markdown-hide-markup nil
	markdown-list-indent-width 4
        markdown-indent-on-enter 'indent-and-new-item)
  :bind (:map markdown-mode-map
	      ("<S-tab>" . markdown-shifttab)))

;;; Org-mode (personal information manager)
(use-package org
  :init
  ;; (setq org-directory (expand-file-name "~/Documents/org/"))
  (setq org-imenu-depth 7)

  (add-to-list 'safe-local-variable-values '(org-hide-leading-stars . t))
  (add-to-list 'safe-local-variable-values '(org-hide-macro-markers . t))
  :bind
  ( :map global-map
    ("C-c l" . org-store-link)
    ("C-c o" . org-open-at-point-global)
    :map org-mode-map
    ;; I don't like that Org binds one zillion keys, so if I want one
    ;; for something more important, I disable it from here.
    ("C-'" . nil)
    ("C-," . nil)
    ("M-;" . nil)
    ("<C-return>" . nil)
    ("<C-S-return>" . nil)
    ("C-M-S-<right>" . nil)
    ("C-M-S-<left>" . nil)
    ("C-c ;" . nil)
    ("C-c M-l" . org-insert-last-stored-link)
    ("C-c C-M-l" . org-toggle-link-display)
    ("M-." . org-edit-special) ; alias for C-c ' (mnenomic is global M-. that goes to source)
    :map org-src-mode-map
    ("M-," . org-edit-src-exit) ; see M-. above
    :map narrow-map
    ("b" . org-narrow-to-block)
    ("e" . org-narrow-to-element)
    ("s" . org-narrow-to-subtree)
    :map ctl-x-x-map
    ("i" . prot-org-id-headlines)
    ("h" . prot-org-ox-html))
  :config
;;;; general settings
  (setq org-ellipsis " ")
  ;; (setq org-adapt-indentation nil)      ; No, non, nein,  χι!
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("t" . "src emacs-lisp :tangle FILENAME")
          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("X" . "export")
          ("q" . "quote")))
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info ol-eww))
  (setq org-use-sub-superscripts '{})
  (setq org-insert-heading-respect-content t)
  (setq org-read-date-prefer-future 'time)
  (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)
  (setq org-priority-faces nil)
  )

;; howm Hitori Otegaru Wiki Modoki -------------------------------------------------------
(use-package howm
  :init
  ;; (define-key global-map [katakana] 'howm-menu) ; [カタカナ] キーでメニュー
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d.org") ; 1 日 1 ファイル
  (setq howm-keyword-case-fold-search t) ; <<< で大文字小文字を区別しない
  (setq howm-list-title t) ; 一覧時にタイトルを表示
  (setq howm-view-title-header "** =")
  (setq howm-directory "~/howm/")
  (setq howm-history-file "~/howm/.howm-history") 
  (setq howm-keyword-file "~/howm/.howm-keys")
  
  ;; Use ripgrep as grep
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)
  ;; git pull remote's howm
  (defun howm-pull-origin ()
    "Pull howm repository remote origin."
    (interactive)
    (let ((default-directory howm-directory) ; 固定のディレクトリを設定
          (display-buffer-alist
           '(("\\*Git Pull Output\\*.*" display-buffer-no-window . nil)))) ; git pullのバッファを表示しない
      (async-shell-command "git pull" "*Git Pull Output*" "*Messages*")))
  ;; keymap
  (setq howm-default-key-table
	'(
	  ;; ("key" func list-mode-p global-p)
	  ("r" howm-refresh)
	  ("l" howm-list-recent t t)
	  ("a" howm-list-all t t)
	  ("g" howm-list-grep t t)
	  ("s" howm-list-grep-fixed t t)
	  ("m" howm-list-migemo t t)
	  ("t" howm-list-todo t t)
	  ("y" howm-list-schedule t t)
	  ("b" howm-list-buffers t t)
	  ("x" howm-list-mark-ring t t)
	  ("o" howm-occur t t)
	  ("c" howm-create t t)
	  ("e" howm-remember t t)
	  ("," howm-menu t t)
	  ("." howm-find-today nil t)
	  (":" howm-find-yesterday nil t)
	  ("A" howm-list-around)
	  ("h" howm-history nil t)
	  ("D" howm-dup)
	  ("i" howm-insert-keyword nil t)
	  ("d" howm-insert-date nil t)
	  ("T" howm-insert-dtime nil t)
	  ("K" howm-keyword-to-kill-ring t t)
	  ("n" action-lock-goto-next-link)
	  ("p" action-lock-goto-previous-link)
	  ("Q" howm-kill-all t t)
	  (" " howm-toggle-buffer nil t)
	  ("N" howm-next-memo)
	  ("p" howm-pull-origin t t)
	  ("P" howm-previous-memo)
	  ("H" howm-first-memo)
	  ("L" howm-last-memo)
	  ("C" howm-create-here nil t)
	  ("I" howm-create-interactively nil t)
	  ("w" howm-random-walk nil t)
	  ("M" howm-open-named-file t t)
	  )
	)
  :config
  ;; remove some default behaviours on howm-related modes
  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)
  )

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

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :init
  (global-diff-hl-mode +1)
  (global-diff-hl-show-hunk-mouse-mode +1))

(use-package git-auto-commit-mode
  :hook
  (howm-mode . git-auto-commit-mode)
  :custom
  (gac-silent-message-p t)
  (gac-automatically-push-p t))
(provide 'init)
;;; init.el ends herke
