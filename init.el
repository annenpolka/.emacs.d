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

;; ╭──────────────────────────────────────────────────────────╮
;; │                           leaf                           │
;; ╰──────────────────────────────────────────────────────────╯
(eval-and-compile
  (customize-set-variable
   'package-archives
   '
   (("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")))
  (package-initialize)

  ;; setup straight.el
  (defvar bootstrap-version)
  (let
      (
       (bootstrap-file
        (expand-file-name
         "straight/repos/straight.el/bootstrap.el"
         user-emacs-directory))
       (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent
           'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  ;; install leaf requirements by straight
  (straight-use-package 'leaf) (straight-use-package 'leaf-keywords)

  (leaf
    leaf-keywords
    :require t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (straight-use-package 'hydra)
    (straight-use-package 'major-mode-hydra)
    (straight-use-package 'blackout)
    (straight-use-package 'system-packages)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; leaf plugins
(leaf
  leaf
  :config (leaf leaf-convert :straight t :require t))

;; garbage collection manager
(leaf gcmh
  :straight t
  :require t
  :blackout t
  :defun (gcmh-mode)
  :custom
  (gcmh-verbose . t)
  :config
  (gcmh-mode 1))

;; explain macro by step
(leaf
  macrostep
  :straight t
  :require t
  :bind (("C-c e" . macrostep-expand)))

;; flycheck syntax checking
(leaf
  flycheck
  :straight t
  :require t
  :global-minor-mode global-flycheck-mode)

;; formatter bindings
(leaf
  format-all
  :straight t
  :require t
  :hook (prog-mode-hook . format-all-mode))

;; EditorConfig support
(leaf
  editorconfig
  :straight t
  :require t
  :global-minor-mode editorconfig-mode)

;; Tangle the code blocks on save.
(defun my/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

;; (leaf flycheck-inline
;;   :straight t :require t
;;   :hook ((flycheck-mode-hook . flycheck-inline-mode)))
;; ╭──────────────────────────────────────────────────────────╮
;; │                       basic, chore                       │
;; ╰──────────────────────────────────────────────────────────╯
;; supress leaf's :custom auto append to custom.el
(leaf
  cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag
  "builtin"
  "faces"
  "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

;; set builtin configs via leaf
(leaf
  cus-start
  :doc "define customization properties of builtins"
  :tag
  "builtin"
  "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))
  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom
  '
  ((user-full-name . "annenpolka")
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
   (completion-cycle-threshold . 3)
   (tab-always-indent . 'complete)
   (scroll-bar-mode . nil)
   (indent-tabs-mode . nil)
   (vc-follow-symlinks . t)
   (show-paren-style . 'mixed))
  :init
  (defalias 'yes-or-no-p 'y-or-n-p)
  (define-key key-translation-map [?\C-h] [?\C-?])
  (global-set-key (kbd "C-?") 'help-for-help))

;; undo
(leaf undo-fu :straight t :require t)
(leaf
  undo-fu-session
  :straight t
  :require t
  :global-minor-mode global-undo-fu-session-mode)

(leaf
  autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1) (auto-revert-check-vc-info . t))
  :global-minor-mode global-auto-revert-mode)

(leaf
  recentf
  :doc "save recent file history"
  :global-minor-mode t
  :custom
  ((recentf-save-file . "~/.emacs.d/recentf")
   (recentf-max-saved-items . 2000)
   (recentf-auto-cleanup . 'never))
  (recentf-exclude
   .
   '
   ("recentf"
    "COMMIT_EDITMSG"
    "bookmarks"
    "\\.gitignore"
    "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
    ".howm-keys"
    "^/tmp/"
    "^/scp:"
    (lambda (file) (file-in-directory-p file package-user-dir)))))

(leaf
  saveplace
  :doc "save cursor position"
  :global-minor-mode save-place-mode)

;;; spellcheck
(leaf
  flyspell
  :blackout (flyspell-mode flyspell-prog-mode)
  :hook
  (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode)
  (conf-mode-hook . flyspell-prog-mode)
  (yaml-mode-hook . flyspell-prog-mode)
  :defvar
  (ispell-extra-args
   ispell-aspell-dict-dir
   ispell-aspell-data-dir
   ispell-program-name)
  :defun (ispell-get-aspell-config-value)
  :config
  (pcase
      (cond
       ((executable-find "aspell")
        'aspell)
       ((executable-find "hunspell")
        'hunspell)
       ((executable-find "enchant-2")
        'enchant))
    (`aspell
     (setq
      ispell-program-name
      "aspell"
      ispell-extra-args '("--sug-mode=ultra" "--run-together"))

     (unless ispell-aspell-dict-dir
       (setq ispell-aspell-dict-dir
             (ispell-get-aspell-config-value "dict-dir")))
     (unless ispell-aspell-data-dir
       (setq ispell-aspell-data-dir
             (ispell-get-aspell-config-value "data-dir"))))
    (`hunspell (setq ispell-program-name "hunspell"))
    (`enchant (setq ispell-program-name "enchant-2"))
    (_ (system-packages-ensure "aspell"))))
(leaf
  flyspell-correct
  :straight t
  :require t
  :after flyspell
  :bind ([remap ispell-word] . flyspell-correct-at-point))

(leaf
  eldoc
  :doc "emacs-lisp documentation"
  :hook (emacs-lisp-mode-hook . turn-on-eldoc-mode)
  :blackout t
  :preface
  (defun my:shutup-eldoc-message (f &optional string)
    (unless (active-minibuffer-window)
      (funcall f string)))
  :advice (:around eldoc-message my:shutup-eldoc-message))

(leaf
  persistent-scratch
  :doc "keep scratch buffer state across sessions"
  :straight t
  :require t
  :defun (persistent-scratch-setup-default)
  :config (persistent-scratch-setup-default))

;; icons dependency
(leaf all-the-icons :straight t :require t)

;; enhanced help
(leaf helpful
  :straight t
  :require t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h k" . helpful-key)
  ("C-h v" . helpful-variable))

;; TODO: create files/projects management section
;; dired extender
(leaf
  dirvish
  :straight t
  :require (all-the-icons dirvish)
  :after evil
  :custom
  ;; Go back home? Just press `bh'
  (dirvish-bookmark-entries
   .
   '
   (("h" "~/" "Home")
    ("d" "~/Downloads/" "Downloads")
    ("e" "~/.emacs.d/" "Emacs")
    ("t" "~/.local/share/Trash/files/" "TrashCan")))
  ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format ; it's ok to place string inside
   .
   '
   (:left
    (sort file-time " " file-size symlink)
    :right (omit yank index)))
  (dirvish-attributes
   .
   '(subtree-state all-the-icons collapse file-size vc-state))
  ;; (dirvish-attributes '(file-size vscode-icon)) ; Feel free to try different combination
  ;; Maybe the icons are too big to your eyes
  ;; (dirvish-all-the-icons-height 0.8)
  ;; In case you want the details at startup like `dired'
  ;; (dirvish-hide-details nil)
  :config
  ;; Place this line under :init to ensure the overriding at startup, see #22
  (dirvish-override-dired-mode)
  ;; (dirvish-peek-mode)
  ;; Dired options are respected except a few exceptions,
  ;; see *In relation to Dired* section above
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  ;; Make sure to use the long name of flags when exists
  ;; eg. use "--almost-all" instead of "-A"
  ;; Otherwise some commands won't work properly
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (evil-make-overriding-map dired-mode-map)
  :hook
  ;; show file preview in minibuffer browsing
  ;; HACK: enabling dirvish-peek-mode in :config somehow won't show preview correctly
  (emacs-startup-hook . dirvish-peek-mode)
  :bind
  (
   (:dired-mode-map
    ("h" . dired-up-directory)
    ;; ("j" . dired-next-line)
    ;; ("k" . dired-previous-line)
    ;; ("l" . dired-find-file)
    ;; ("i" . wdired-change-to-wdired-mode)
    ;; ("." . dired-omit-mode)
    ("TAB" . dirvish-subtree-toggle)
    ("SPC" . dirvish-history-jump)
    ("M-n" . dirvish-history-go-forward)
    ("M-p" . dirvish-history-go-backward)
    ("M-s" . dirvish-setup-menu)
    ("M-f" . dirvish-toggle-fullscreen)
    ("*" . dirvish-mark-menu)
    ("r" . dirvish-fd-roam)
    ("b" . dirvish-bookmark-goto)
    ("f" . dirvish-file-info-menu)
    ("y" . dirvish-yank)
    ("/" . dirvish-fd)
    ([remap dired-sort-toggle-or-edit] . dirvish-quicksort)
    ([remap dired-do-redisplay] . dirvish-ls-switches-menu)
    ([remap dired-summary] . dirvish-dispatch)
    ([remap dired-do-copy] . dirvish-yank-menu)
    ([remap mode-line-other-buffer] . dirvish-history-last))))

;; project management
(leaf
  projectile
  :straight t
  :require t
  :bind (:projectile-mode-map ("C-c p" . projectile-command-map))
  :global-minor-mode t)

;; sessions as bookmark
(leaf burly
  :straight
  (burly
   :type git
   :host github
   :repo "alphapapa/burly.el"))

;; ╭──────────────────────────────────────────────────────────╮
;; │                      language tools                      │
;; ╰──────────────────────────────────────────────────────────╯
;;; lsp
(leaf
  lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf
     (alist-get
      'styles
      (alist-get 'lsp-capf completion-category-defaults))
     '(orderless))) ;; Configure orderless
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (c++-mode-hook . lsp)
  :custom
  ((lsp-idle-delay . 0.5)
   (lsp-log-io . t)
   (lsp-completion-provider . :none)
   (lsp-keymap-prefix . "C-c l")))

;; TODO: setup lsp ui tools
(leaf
  lsp-ui
  :doc "UI modules for lsp-mode"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :straight t
  :require t
  :after lsp-mode
  :custom
  ((lsp-ui-doc-enable . t)
   (lsp-ui-doc-deley . 0.5)
   (lsp-ui-doc-header . t)
   (lsp-ui-doc-include-signature . t)
   (lsp-ui-doc-position . 'at-point)
   (lsp-ui-doc-max-width . 150)
   (lsp-ui-doc-max-height . 30)
   (lsp-ui-doc-use-childframe . nil)
   (lsp-ui-doc-use-webkit . nil)
   (lsp-ui-flycheck-enable . t)
   (lsp-ui-peek-enable . t)
   (lsp-ui-peek-peek-height . 20)
   (lsp-ui-peek-list-width . 50)
   (lsp-ui-peek-fontify . 'on-demand) ;; never, on-demand, or always
   )
  :hook ((lsp-mode-hook . lsp-ui-mode)))

;; TODO: add cpp clangd setup
;; TODO: add rust-mode setup

;; ╭──────────────────────────────────────────────────────────╮
;; │                          themes                          │
;; ╰──────────────────────────────────────────────────────────╯
;; font config
(set-face-attribute 'default nil :font "Iosevka Term-14")
(set-face-attribute 'fixed-pitch nil :family "Iosevka Term" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "PlemolJP Console NF" :height 1.0)

;; modus theme
(leaf modus-themes
  :custom
  (modus-themes-completions . 'subtle)
  (modus-themes-fringes . 'subtle)
  (modus-themes-italic-constructs . t)
  (modus-themes-bold-constructs . nil)
  (modus-themes-mode-line . '(borderless moody))
  (modus-themes-hl-line . '(underline))
  (modus-themes-region . '(bg-only no-extend))
  (modus-themes-scale-headings . t)
  (modus-themes-prompts . '(background bold gray intense italic))
  (modus-themes-syntax . '(faint alt-syntax green-strings))
  :config (load-theme 'modus-vivendi))
;; ╭──────────────────────────────────────────────────────────╮
;; │                     helper interface                     │
;; ╰──────────────────────────────────────────────────────────╯
(leaf
  display-line-numbers
  :bind ("<f9>" . display-line-numbers-mode)
  :hook ((prog-mode-hook text-mode-hook) . display-line-numbers-mode)
  :custom (display-line-numbers-width . 4))

(leaf hl-line :global-minor-mode global-hl-line-mode)

(leaf
  which-key
  :doc "which-key in emacs"
  :straight t
  :require t
  :defun (which-key-setup-side-window-right which-key-mode)
  :blackout which-key-mode
  :custom (which-key-idle-delay . 0.5)
  :init
  (which-key-setup-side-window-right)
  (which-key-mode t))

(leaf
  hl-todo
  :doc "TODO keywords highlighting"
  :straight t
  :require t
  :global-minor-mode global-hl-todo-mode)

(leaf
  highlight-indent-guides
  :doc "indent lines"
  :straight t
  :require t
  :blackout t
  :hook
  (((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode))
  :custom
  ((highlight-indent-guides-method . 'character)
   (highlight-indent-guides-auto-enabled . t)
   (highlight-indent-guides-responsive . t)))

(leaf
  hl-block-mode
  :doc "blockman thing"
  :straight t
  :require t
  :global-minor-mode global-hl-block-mode)

(leaf
  rainbow-delimiters
  :straight t
  :require t
  :hook ((prog-mode-hook org-mode-hook) . rainbow-delimiters-mode))

;; Code folding
(leaf
  origami
  :straight t
  :require t
  :after evil
  :global-minor-mode global-origami-mode)

;; focus window
(leaf
  zoom
  :straight t
  :ensure t
  :blackout t
  :global-minor-mode zoom-mode
  ;; TODO: set ignore major modes like dired
  :custom (zoom-size . '(0.618 . 0.618)))

;; ╭─────-────────────────────────────────────────────────────╮
;; │                      editing modal                       │
;; ╰──────────────────────────────────────────────────────────╯

(leaf
  evil
  :doc "Extensible vi layer for Emacs."
  :straight t
  :require
  evil
  windmove
  undo-fu
  :defun
  (evil-ex-nohighlight
   evil-mc-undo-all-cursors
   evil-set-leader
   evil-mode
   turn-on-evil-mode
   evil-define-command
   evil-define-key)
  :defvar (evil-want-keybinding evil-undo-system)
  :init
  (defun my/clear-marks-and-cursors ()
    (interactive)
    (evil-ex-nohighlight)
    (evil-mc-undo-all-cursors))
  :pre-setq
  ;; for evil-collection
  (evil-want-keybinding . nil)
  ;; undo system
  (evil-undo-system . 'undo-fu)
  :custom
  ;; <C-u> to scroll (replace universal-argument)
  (evil-want-C-u-scroll . t)
  ;; serach module
  (evil-search-module 'evil-search)
  :config
  ;; leader-key
  (evil-set-leader 'normal (kbd "<SPC>"))
  (evil-set-leader 'visual (kbd "<SPC>"))
  ;; activate evil
  (evil-mode 1)
  (turn-on-evil-mode)
  :bind
  (
   (:evil-normal-state-map
    ("C-s" . save-buffer)
    ("C-q" . 'evil-quit)
    ("C-l" . 'my/clear-marks-and-cursors)
    ("K" . 'helpful-at-point)
    ("C-j" . 'evil-open-fold)
    ("C-k" . 'evil-close-fold)
    ("C-/" . 'evil-commentary-line)
    ("zg" . flyspell-correct-at-point)
    ("C-e" . find-file)
    ("C-f" . consult-line)
    ("<leader>SPC" . 'consult-buffer)
    ("<leader>n" . 'my-mc-hydra/body)
    ("<leader>g" . 'my-git-actions/body)
    ("<leader>h" . 'move-or-create-window-left)
    ("<leader>j" . 'move-or-create-window-below)
    ("<leader>k" . 'move-or-create-window-above)
    ("<leader>l" . 'move-or-create-window-right))
   (:evil-visual-state-map
    ("<leader>n". 'my-mc-hydra/body))))

;; vim-textobj-user
(leaf
  targets
  :straight
  (targets
   :type git
   :host github
   :repo "noctuid/targets.el")
  :require t
  :defun (targets-setup targets-define-composite-to)
  :config
  (targets-setup)
  ;; vim-textobj-anyblock
  (targets-define-composite-to anyblock
    (("(" ")" pair)
     ("[" "]" pair)
     ("{" "}" pair)
     ("<" ">" pair)
     ("\"" "\"" quote)
     ("'" "'" quote)
     ("`" "`" quote)
     ("“" "”" quote))
    :bind t
    :keys "b"))

;; indent textobject
(leaf
  evil-indent-plus
  :straight t
  :require t
  :bind
  ((:evil-inner-text-objects-map
    ("i" . 'evil-indent-plus-i-indent-up)
    ("I" . 'evil-indent-plus-i-indent))
   (:evil-outer-text-objects-map
    ("i" . 'evil-indent-plus-a-indent-up)
    ("I" . 'evil-indent-plus-a-indent))))

(leaf
  evil-collection
  :after evil
  :straight t
  :require t
  :defun (evil-collection-init)
  :custom
  (evil-collection-setup-minibuffer . t)
  (evil-collection-want-unimpaired-p . nil)
  :config
  ;; (evil-collection-init '(magit dired consult)))
  (evil-collection-init))

(leaf
  evil-commentary
  :after evil
  :straight t
  :require t
  :blackout t
  :global-minor-mode evil-commentary-mode)

(leaf
  smartparens
  :straight t
  :require smartparens-config
  :blackout t
  :global-minor-mode smartparens-global-mode)
(leaf
  evil-smartparens
  :straight t
  :require t
  :after smartparens
  :blackout t
  :hook ((smartparens-enabled-hook . evil-smartparens-mode)))

(leaf embrace :straight t :require t)
(leaf evil-embrace :straight t :require t)
(leaf
  evil-surround
  :straight t
  :require
  embrace
  evil-embrace
  evil-surround
  :defun
  (global-evil-surround-mode
   evil-embrace-enable-evil-surround-integration)
  :config
  (global-evil-surround-mode 1)
  (evil-embrace-enable-evil-surround-integration))

(leaf
  evil-mc
  :after evil
  :straight t
  :require (smartparens evil-smartparens)
  :global-minor-mode global-evil-mc-mode
  :config
  (evil-define-command
    evil-mc-toggle-cursor-here
    ()
    "Create a cursor at point. If in visual block or line mode, then create
   cursors on each line of the selection, on the column of the cursor. Otherwise
   pauses cursors."
    :repeat nil
    :keep-visual nil
    :evil-mc
    t
    (cond
     (
      (and
       (evil-mc-has-cursors-p) (evil-normal-state-p)
       (let*
           (
            (pos (point))
            (cursor
             (cl-find-if
              (lambda (cursor)
                (eq pos (evil-mc-get-cursor-start cursor)))
              evil-mc-cursor-list)))
         (when cursor
           (evil-mc-delete-cursor cursor)
           (setq evil-mc-cursor-list
                 (delq cursor evil-mc-cursor-list))
           t))))

     ((memq evil-this-type '(block line))
      (let
          (
           (col (evil-column))
           (line-at-pt (line-number-at-pos)))
        ;; Fix off-by-one error
        (when (= evil-visual-direction 1)
          (cl-decf col)
          (backward-char))
        (save-excursion
          (evil-apply-on-block
           (lambda (ibeg _)
             (unless
                 (or
                  (= line-at-pt (line-number-at-pos ibeg))
                  (invisible-p ibeg))
               (goto-char ibeg)
               (move-to-column col)
               (when (= (current-column) col)
                 (evil-mc-make-cursor-here))))
           evil-visual-beginning
           (if (eq evil-this-type 'line)
               (1- evil-visual-end)
             evil-visual-end)
           nil)
          (evil-exit-visual-state))))
     (t
      (evil-mc-pause-cursors)
      ;; I assume I don't want the cursors to move yet
      (evil-mc-make-cursor-here))))

  (evil-define-command
    evil-mc-undo-cursor
    ()
    "Undos last cursor, or all cursors in visual region."
    :repeat nil
    :evil-mc
    t
    (if (evil-visual-state-p)
        (or
         (mapc
          (lambda (c)
            (evil-mc-delete-cursor c)
            (setq evil-mc-cursor-list (delq c evil-mc-cursor-list)))
          (cl-remove-if-not
           (lambda (pos)
             (and
              (>= pos evil-visual-beginning)
              (< pos evil-visual-end)))
           evil-mc-cursor-list
           :key #'evil-mc-get-cursor-start))
         (message "No cursors to undo in region"))
      (evil-mc-undo-last-added-cursor)))

  ;; smartparens integration
  (dolist
      (sp-command
       '
       (sp-up-sexp
        sp-copy-sexp
        sp-down-sexp
        sp-join-sexp
        sp-kill-sexp
        sp-next-sexp
        sp-split-sexp
        sp-wrap-curly
        sp-wrap-round
        sp-raise-sexp
        sp-clone-sexp
        sp-wrap-square
        sp-splice-sexp
        sp-end-of-sexp
        sp-forward-sexp
        sp-backward-sexp
        sp-convolute-sexp
        sp-transpose-sexp
        sp-kill-whole-line
        sp-beginning-of-sexp
        sp-forward-barf-sexp
        sp-forward-slurp-sexp
        sp-backward-barf-sexp
        sp-backward-slurp-sexp
        sp-splice-sexp-killing-forward
        sp-splice-sexp-killing-backward))
    (add-to-list 'evil-mc-custom-known-commands
                 `(,sp-command (:default . evil-mc-execute-call))))
  :hydra
  (my-mc-hydra
   (:color pink :hint nil :pre (evil-mc-pause-cursors))
   "
    ^Match^            ^Line-wise^           ^Manual^
    ^^^^^^----------------------------------------------------
    _Z_: match all     _J_: make & go down   _z_: toggle here
    _m_: make & next   _K_: make & go up     _r_: remove last
    _M_: make & prev   ^ ^                   _R_: remove all
    _n_: skip & next   ^ ^                   _p_: pause
    _N_: skip & prev   ^ ^                   _P_: resume

    Current pattern: %`evil-mc-pattern

    "
   ("Z" #'evil-mc-make-all-cursors)
   ("m" #'evil-mc-make-and-goto-next-match)
   ("M" #'evil-mc-make-and-goto-prev-match)
   ("n" #'evil-mc-skip-and-goto-next-match)
   ("N" #'evil-mc-skip-and-goto-prev-match)
   ("J" #'evil-mc-make-cursor-move-next-line)
   ("K" #'evil-mc-make-cursor-move-prev-line)
   ("z" #'evil-mc-toggle-cursor-here)
   ("r" #'evil-mc-undo-cursor)
   ("R" #'evil-mc-undo-all-cursors)
   ("p" #'evil-mc-pause-cursors)
   ("P" #'evil-mc-resume-cursors)
   ("q" #'evil-mc-resume-cursors "quit" :color blue)
   ("Q"
    #'evil-mc-undo-all-cursors
    "quit with remove all"
    :color blue)))

(leaf
  evil-escape
  :after evil
  :straight t
  :require t
  :blackout t
  :global-minor-mode evil-escape-mode
  :setq-default
  (evil-escape-key-sequence . "jk")
  (evil-escape-delay . 0.2)
  (evil-escape-excluded-states . '(normal visual motion emacs))
  (evil-escape-excluded-major-modes
   .
   '
   (magit-status-mode
    magit-revision-mode magit-diff-mode help-mode)))

;; display registers
(leaf evil-owl
  :straight t
  :require t
  :global-minor-mode evil-owl-mode
  :custom
  (evil-owl-max-string-length . 500)
  (evil-owl-idle-delay . 0.2)
  :config
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25))))

;; org mode things
(leaf org
  :straight t
  :require org org-tempo
  :custom
  (org-catch-invisible-edits . 'smart)
  :config
  (defun disable-flycheck-in-org-src-block ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'org-src-mode-hook 'disable-flycheck-in-org-src-block))

(leaf org-bullets
  :straight t
  :require t
  :after org
  :hook (org-mode-hook . org-bullets-mode)
  :custom
  (org-bullets-bullet-list . '("◉" "○" "●" "○" "●" "○" "●")))

(leaf
  evil-org
  :straight t
  :require t
  :hook (org-mode-hook . evil-org-mode)
  :defun (evil-org-agenda-set-keys)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key '(normal visual) 'evil-org-mode
    (kbd "C-a") 'org-edit-special
    (kbd "C-j") 'org-next-visible-heading
    (kbd "C-k") 'org-previous-visible-heading
    (kbd "C-S-j") 'org-move-subtree-down
    (kbd "C-S-k") 'org-move-subtree-up))

(leaf move-or-create-window
  :doc "focus.nvim in emacs"
  :init
  (defun move-or-create-window-above nil
    (interactive)
    (if (window-in-direction 'above)
        (windmove-up)
      (progn
        (split-window-below)
        (windmove-up))))

  (defun move-or-create-window-below nil
    (interactive)
    (if (window-in-direction 'below)
        (windmove-down)
      (progn
        (split-window-below)
        (windmove-down))))

  (defun move-or-create-window-left nil
    (interactive)
    (if (window-in-direction 'left)
        (windmove-left)
      (progn
        (split-window-right)
        (windmove-left))))

  (defun move-or-create-window-right nil
    (interactive)
    (if (window-in-direction 'right)
        (windmove-right)
      (progn
        (split-window-right)
        (windmove-right))))
  )

;; ╭──────────────────────────────────────────────────────────╮
;; │                           Git                            │
;; ╰──────────────────────────────────────────────────────────╯
(leaf
  magit
  :doc "great git client"
  :straight t
  :require t
  :pretty-hydra
  (my-git-actions
   (:color pink :separator "=" :quit-key "q")
   ("Movement"
    (("J" diff-hl-next-hunk "next hunk")
     ("K" diff-hl-previous-hunk "previous hunk"))
    "Diff"
    (("D" diff-hl-show-hunk "diff nearest hunk")
     ("N" diff-hl-show-hunk-next "diff next hunk")
     ("P" diff-hl-show-hunk-previous "diff previous hunk"))
    "Operation"
    (("r" diff-hl-revert-hunk "revert hunk")
     ("s" diff-hl-stage-current-hunk "stage hunk")
     ("U" diff-hl-unstage-file "unstage all"))
    "Magit"
    (("<RET>" magit-status "open magit" :color blue)
     ("C" magit-commit "commit" :color blue)))))

(leaf
  forge
  :doc "remote repo control with magit"
  :straight t
  :require t
  :after magit
  :custom
  ((bug-reference-mode . 0) (forge-add-default-bindings . nil)))

(leaf
  magit-todos
  :doc "manage TODO keywords with magit"
  :straight t
  :require t
  :after magit
  :hook (magit-mode-hook . magit-todos-mode))

(leaf
  diff-hl
  :straight t
  :require t
  :hook
  ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :global-minor-mode global-diff-hl-mode
  :custom (diff-hl-show-staged-changes . nil))

;; ╭──────────────────────────────────────────────────────────╮
;; │                        completion                        │
;; ╰──────────────────────────────────────────────────────────╯

;; ----- minibuffer -----
(leaf
  vertico
  :straight t
  :require t
  :global-minor-mode vertico-mode
  :custom ((vertico-cycle . t)))

;; completion style
(leaf
  fussy
  :straight t
  :require t
  :init
  (leaf
    fuz-bin
    :straight
    '
    (fuz-bin
     :repo "jcs-elpa/fuz-bin"
     :fetcher github
     :files (:defaults "bin"))
    :require t
    :defun (fuz-bin-load-dyn)
    :config (fuz-bin-load-dyn))
  :custom
  ((completion-styles . '(fussy))
   (completion-category-defaults . nil)
   (compleiton-category-overrides . nil)
   (fussy-filter-fn . 'fussy-filter-fast)
   (fussy-score-fn . 'fussy-fuz-bin-score)
   (fussy-fuz-use-skim-p . t)))

(leaf
  savehist
  :straight t
  :require t
  :defvar (savehist-coding-system)
  :init (savehist-mode)
  :setq (savehist-coding-system . 'utf-8-emacs))

;;; marginalia
(leaf
  marginalia
  :straight t
  :require t
  :defvar (marginalia-mode)
  :global-minor-mode marginalia-mode)

;;; embark
(leaf embark :straight t :require t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  )

(leaf
  consult
  :straight t
  :require t
  :defvar (consult-buffer-sources)
  :setq
  (
   (completion-in-region-function
    .
    (lambda (&rest args)
      (apply
       (if vertico-mode
           #'consult-completion-in-region
         #'completion--in-region)
       args)))
   (consult-buffer-sources
    .
    '
    (consult--source-hidden-buffer
     consult--source-buffer
     consult--source-project-buffer
     consult--source-bookmark
     consult--source-recent-file))))

;; dir extension
(leaf
  consult-dir
  :straight t
  :require t
  :after consult
  :bind
  (("C-x C-d" . consult-dir)
   (:vertico-map
    ("C-x C-d" . consult-dir)
    ("C-x C-j" . consult-dir-jump-file)))
  :custom
  (consult-dir-project-list-function . #'consult-dir-projectile-dirs))

;; affe fuzzy-finder
(leaf
  affe
  :straight t
  :require t
  :init
  (leaf orderless :straight t :commands (orderless-pattern-compiler))
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

;; ----- auto completion -----
(leaf
  corfu
  ;; :disabled t ;; try company
  :after evil
  :straight t
  :require t
  :init
  (defun corfu-enable-in-minibuffer nil
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when
        (where-is-internal
         #'completion-at-point
         (list (current-local-map)))
      (corfu-mode 1)))
  :hook
                                        ; (minibuffer-setup-hook . corfu-enable-in-minibuffer)
  (after-init-hook . global-corfu-mode)
  :custom
  (corfu-cycle . t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto . t) ;; Enable auto completion
  (corfu-auto-delay . 0.1) ;; Enable auto completion
  (corfu-count . 15) ;; show more candidates
  (corfu-quit-at-boundary . t) ;; nil: スペースを入れてもquitしない
  ;; (corfu-quit-no-match . nil) ;; nil: マッチしないとき"no match"を表示してquitしない
  (corfu-auto-prefix . 1)
  (corfu-preview-current . t) ;; current candidate preview
  (corfu-preselect-first . t) ;; candidate preselection
  (corfu-quit-no-match . 'separator)
  (corfu-separator . ?\s)
  :bind
  (
   (:corfu-map
    ("TAB" . corfu-next)
    ("<tab>" . corfu-next)
    ("S-TAB" . corfu-previous)
    ("<backtab>" . corfu-previous)
    ("C-n" . corfu-next)
    ("C-p" . corfu-previous)))
  :config
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps))

(leaf
  corfu-doc
  :straight t
  :require t
  :hook (corfu-mode-hook . corfu-doc-mode))

(leaf
  yasnippet
  :straight t
  :require t
  :bind
  (:yas-keymap
   ("<tab>" . nil)
   ("TAB" . nil)
   ("<backtab>" . nil)
   ("S-TAB" . nil)
   ("C-o" . yas-next-field-or-maybe-expand))
  :global-minor-mode yas-global-mode)

(leaf
  cape
  :straight t
  :require t
  :init
  (defun my/basic-super-capf ()
    (add-to-list 'completion-at-point-functions
                 (cape-super-capf
                  ;; #'lsp-completion-at-point
                  #'cape-file
                  #'cape-tex
                  #'cape-symbol
                  #'cape-keyword
                  #'cape-dabbrev
                  #'cape-abbrev
                                        ;#'cape-ispell
                                        ;#'cape-dict
                                        ;#'cape-line
                  )))
  :hook (prog-mode-hook . my/basic-super-capf))


(leaf
  kind-icon
  :straight t
  :require t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ╭──────────────────────────────────────────────────────────╮
;; │                       input method                       │
;; ╰──────────────────────────────────────────────────────────╯
;; mozc ime
(leaf
  mozc
  ;; :depends emacs-mozc
  :straight t
  :require t
  :bind* (("<zenkaku-hankaku>" . toggle-input-method)
          ("<eisu-toggle>" . toggle-input-method))
  :custom
  ((default-input-method . "japanese-mozc")
   (mozc-candidate-style . 'overlay))
  )

;; ╭──────────────────────────────────────────────────────────╮
;; │                       boilerplate                        │
;; ╰──────────────────────────────────────────────────────────╯
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
