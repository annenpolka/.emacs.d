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

  (leaf leaf-keywords
    :require t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (straight-use-package 'hydra)
    (straight-use-package 'major-mode-hydra)
    (straight-use-package 'blackout)
    (straight-use-package 'system-packages)
    ;; use bind-key for modal-depending keymapping
    (straight-use-package 'bind-key)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; leaf plugins
(leaf leaf
  :config (leaf leaf-convert :straight t :require t))

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
   (select-enable-primary . nil)
   (show-paren-style . 'parenthesis))
  :init
  (defalias 'yes-or-no-p 'y-or-n-p)
  (define-key key-translation-map [?\C-h] [?\C-?])
  (global-set-key (kbd "C-?") 'help-for-help))

(leaf
  cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag
  "builtin"
  "faces"
  "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

;; Tangle the code blocks on save.
(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "emacs.org" user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

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

(leaf restart-emacs
  :straight t
  :require t)

(set-face-attribute 'default nil :font "Iosevka Term-14")
(set-face-attribute 'fixed-pitch nil :family "Iosevka Term" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "MigMix 1M" :height 1.0)

;; Japanese font
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "Migmix 1M"))

;; modus theme
(leaf modus-themes
  :custom
  (modus-themes-completions . 'subtle)
  (modus-themes-fringes . 'subtle)
  (modus-themes-italic-constructs . t)
  (modus-themes-bold-constructs . nil)
  (modus-themes-tabs-accented . nil)
  (modus-themes-mode-line . '(borderless))
  (modus-themes-hl-line . '(underline))
  (modus-themes-region . '(bg-only no-extend))
  (modus-themes-scale-headings . t)
  (modus-themes-prompts . '(background bold gray intense italic))
  (modus-themes-syntax . '(faint alt-syntax green-strings))
  :config (load-theme 'modus-vivendi))

;; icons dependency
(leaf all-the-icons :straight t :require t
  :custom
  (all-the-icons-scale-factor . 1.1)
  )

(leaf
  kind-icon
  :straight t
  :require t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf dashboard
  :straight t
  :require t
  :custom
  (dashboard-banner-logo-title . "Kurumi Emacs")
  (dashboard-startup-banner . 'logo)
  (dashboard-footer-messages .
                             '("I'm a thinker, I could break it down."
                               "I'm a shooter. A drastic baby."
                               "Agitate and jump out. Feel it in the will."
                               "Can you talk about deep-sea with me."
                               "The deep-sea fish loves you forever. All are as your thinking over."
                               "Out of space, when someone waits there."
                               "Sound of jet, they played for out."))
  (dashboard-projects . 'projectile)
  (dashboard-items . '((recents  . 5)
                       (bookmarks . 5)
                       (projects . 5)))
  :config
  (setq dashboard-footer-icon (all-the-icons-material "dashboard"
                                                      :height 1.0
                                                      :v-adjust -0.05
                                                      :face 'font-lock-keyword-face))
  (dashboard-setup-startup-hook))

(leaf doom-modeline
  :straight t
  :require t
  :init
  :hook
  (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes . nil)
  (doom-modeline-major-mode-icon . t)
  (doom-modeline-bar-width . 6)
  (doom-modeline-height . 15)
  (doom-modeline-modal-icon . nil)
  (doom-modeline-icon . t)
  :config
  (custom-set-faces
   '(mode-line ((t (:family "Hack" :height 0.9))))
   ;; '(mode-line-active ((t (:family "Hack" :height 0.9 :weight light))))
   '(mode-line-inactive ((t (:family "Hack" :height 0.9)))))

  ;; Define your custom doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info lsp checker)
    '(misc-info minor-modes input-method buffer-encoding major-mode process vcs " ")) ; <-- added padding here
  (doom-modeline-def-modeline 'org-src
    '(bar matches buffer-info-simple buffer-position parrot selection-info checker)
    '(misc-info minor-modes lsp input-method buffer-encoding major-mode process vcs " ")) ; <-- added padding here
  )

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
  :init
  (defun off-input-method nil
    "deactivate multilingual input method."
    (interactive)
    (deactivate-input-method))
  (defun on-input-method nil
    "activate multilingual input method."
    (interactive)
    (activate-input-method default-input-method))
  )

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
  display-line-numbers
  :bind ("<f9>" . display-line-numbers-mode)
  :hook ((prog-mode-hook text-mode-hook) . display-line-numbers-mode)
  :custom (display-line-numbers-width . 4))

(leaf hl-line :global-minor-mode global-hl-line-mode)

(leaf
  hl-block-mode
  :doc "blockman thing"
  :straight t
  :require t
  :global-minor-mode global-hl-block-mode)

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
  rainbow-delimiters
  :straight t
  :require t
  :hook ((prog-mode-hook org-mode-hook) . rainbow-delimiters-mode))

(leaf rainbow-mode
  :straight t
  :require t
  :blackout t
  :hook
  (prog-mode-hook . rainbow-mode)
  (org-mode-hook . rainbow-mode))

;; focus window
(leaf
  zoom
  :straight t
  :ensure t
  :blackout t
  :global-minor-mode zoom-mode
  :custom (zoom-size . '(0.618 . 0.618)))

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

(leaf centered-cursor-mode
  :straight t
  :require t
  :blackout t
  :global-minor-mode global-centered-cursor-mode
  :custom (ccm-step-size . 2))

;; enhanced help
(leaf helpful
  :straight t
  :require t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h k" . helpful-key)
  ("C-h v" . helpful-variable))

(leaf
  dirvish
  :straight t
  :require (all-the-icons dirvish)
  ;; :after evil
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
  ;; (evil-make-overriding-map dired-mode-map)
  :hook
  ;; show file preview in minibuffer browsing
  ;; HACK: enabling dirvish-peek-mode in :config somehow won't show preview correctly
  (emacs-startup-hook . dirvish-peek-mode)
  :bind
  (
   (:dirvish-mode-map
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
  :custom
  (diff-hl-show-staged-changes . nil)
  (diff-hl-ask-before-revert-hunk . t))

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

(leaf meow
  :straight t
  :require t
  :init
  ;; key-chord dependency
  (leaf key-chord
    :straight t
    :require t
    :global-minor-mode t
    :custom
    (key-chord-two-keys-delay . 0.1)
    (key-chord-one-keys-delay . 0.2)
    )
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
  ;; setup keymap
  (defun meow-setup nil
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; move-or-create-window prefix
     '("w j" . move-or-create-window-below)
     '("w k" . move-or-create-window-above)
     '("w h" . move-or-create-window-left)
     '("w l" . move-or-create-window-right)
     ;; version control operations
     '("v" . my-git-actions/body)
     ;; spell correction
     '("z g" . flyspell-correct-at-point)
     ;; help
     '("K" . helpful-at-point)
     ;; consult search operations
     '("SPC" . consult-buffer)
     '("s p" . consult-ripgrep)
     ;; Use SPC (0-9) for digit arguments.
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
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . meow-find-backward)
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
     ;; '("r" . embrace-commander)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("S" . embrace-commander)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . undo-redo)
     '("C-r" . undo-redo)
     '("v" . meow-visit)
     '("V" . er/expand-region)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("/" . consult-line)
     '("C-u" . ccm-scroll-down)
     '("C-d" . ccm-scroll-up)
     '("C-o" . better-jumper-jump-backward)
     '("C-i" . better-jumper-jump-forward)
     '("C-s" . save-buffer)
     ;; '("TAB" . origami-toggle-node)
     '("<tab>" . origami-toggle-node)
     '("C-s" . save-buffer)
     '("<escape>" . ignore))
    ;; insert state keymap
    (meow-define-keys
        'insert
      '("C-g" . meow-insert-exit)
      '("C-w" . backward-kill-word)
      )
    (key-chord-define meow-insert-state-keymap "jk" 'meow-insert-exit)
    ;; anyblock thing object
    (meow-thing-register 'anyblock
                         '(
                           (#'meow--inner-of-string)
                           (pair ("[") ("]"))
                           (pair ("(") (")"))
                           (pair ("{") ("}"))
                           (pair ("<") (">"))
                           )
                         '(
                           (#'meow--bounds-of-string)
                           (pair ("[") ("]"))
                           (pair ("(") (")"))
                           (pair ("{") ("}"))
                           (pair ("<") (">"))
                           )
                         )
    )
  :bind
  ;; readline style keymap for minibuffer
  (:minibuffer-local-map
   ("C-w" . backward-kill-word))
  :custom
  (meow-use-clipboard . t)
  (meow-mode-state-list . '((helpful-mode . normal)
                            (Man-mode . normal)
                            ))
  (meow--kbd-delete-char . "<deletechar>")
  (meow-selection-command-fallback .
                                   '((meow-reverse . back-to-indentation)
                                     (meow-change . meow-change-char)
                                     (meow-save . meow-save-line)
                                     (meow-kill . meow-kill-whole-line)
                                     (meow-pop-selection . meow-pop-grab)
                                     (meow-beacon-change . meow-beacon-change-char)
                                     (meow-cancel . keyboard-quit)
                                     (meow-delete . meow-C-d)))
  (meow-char-thing-table .
                         '((?r . round)
                           (?b . anyblock) ;; `b' for bracket
                           (?c . curly)
                           (?s . string)
                           (?e . symbol)
                           (?w . window)
                           (?B . buffer)
                           (?p . paragraph)
                           (?\[ . square)
                           (?\( . round)
                           (?l . line)
                           (?d . defun)
                           (?. . sentence)))
  :config
  (meow-setup)
  ;; vim-way cursor
  ;; Must set before enable `meow-global-mode`
  ;; (setq meow-use-cursor-position-hack t
  ;;       meow-use-enhanced-selection-effect t)  ;; optional, for visual effect
  (meow-global-mode 1)
  )

(leaf expand-region
    :straight t
    :require t)

(leaf
  smartparens
  :straight t
  :require t smartparens-config
  :blackout t
  :global-minor-mode smartparens-global-mode)

(leaf embrace
  :straight t
  :require t)

(leaf better-jumper
  :straight t
  :require t
  :global-minor-mode t
  :custom
  (better-jumper-context . 'window)
  :config
  ;; this is the key here. This advice makes it so you only set a jump point
  ;; if you move more than one line with whatever command you call. For example
  ;; if you add this advice around evil-next-line, you will set a jump point
  ;; if you do 10 j, but not if you just hit j. I did not write this code, I
  ;; I found it a while back and updated it to work with better-jumper.
  (defun my-jump-advice (oldfun &rest args)
    (let ((old-pos (point)))
      (apply oldfun args)
      (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point))))
               1)
        (better-jumper-set-jump old-pos))))

  ;; jump scenarios
  (advice-add 'meow-search :around #'my-jump-advice)
  (advice-add 'meow-visit :around #'my-jump-advice)
  (advice-add 'consult--jump :around #'my-jump-advice)
  (advice-add 'ccm-scroll-up :around #'my-jump-advice)
  (advice-add 'ccm-scroll-down :around #'my-jump-advice)
  ;; (advice-add 'evil-goto-definition :around #'my-jump-advice)
  ;; (advice-add 'evil-goto-mark  :around #'my-jump-advice)
  )

;; undo
(leaf undo-fu :straight t :require t)
(leaf
  undo-fu-session
  :straight t
  :require t
  :global-minor-mode global-undo-fu-session-mode)

(leaf super-save
  :straight t
  :require t
  :blackout t
  :global-minor-mode super-save-mode)

;; Code folding
(leaf
  origami
  :straight t
  :require t
  ;; :after evil
  :global-minor-mode global-origami-mode)

(leaf sidekick
  :straight (sidekick
             :type git
             :host github
             :repo "VernonGrant/sidekick.el")
  :require t ;; sidekick-evil
  :bind
  ;; (:evil-normal-state-map
  ;;  ("<leader>sn" . 'sidekick-at-point))
  )

(leaf avy
  :straight t
  :require t
  :custom
  (avy-timeout-seconds . 0.3)
  (avy-orders-alist .
    '((avy-goto-char-0 . avy-order-closest)
      (avy-goto-word-0 . avy-order-closest)))
  )

;; flycheck syntax checking
(leaf
  flycheck
  :straight t
  :require t
  :global-minor-mode global-flycheck-mode
  :custom
  (flycheck-display-errors-delay . 0.2))

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

  :bind (([remap ispell-word] . flyspell-correct-at-point)
         ;; (:evil-normal-state-map
         ;;  ("zg" . flyspell-correct-at-point)
         ;;  )
         ))

(leaf
  eldoc
  :doc "hovered element documentation"
  :hook (emacs-lisp-mode-hook . turn-on-eldoc-mode)
  :blackout t
  :preface
  (defun my:shutup-eldoc-message (f &optional string)
    (unless (active-minibuffer-window) (funcall f string)))
  :advice (:around eldoc-message my:shutup-eldoc-message))

;; formatter bindings
(leaf
  format-all
  :straight t
  :require t
  :blackout t
  :hook
  (prog-mode-hook . format-all-mode)
  (prog-mode-hook . format-all-ensure-formatter)
  :custom
  (format-all-default-formatters .
                                 '(("Assembly" asmfmt)
                                  ("ATS" atsfmt)
                                  ("Bazel" buildifier)
                                  ("BibTeX" emacs-bibtex)
                                  ("C" clang-format)
                                  ("C#" clang-format)
                                  ("C++" clang-format)
                                  ("Cabal Config" cabal-fmt)
                                  ("Clojure" zprint)
                                  ("CMake" cmake-format)
                                  ("Crystal" crystal)
                                  ("CSS" prettier)
                                  ("Cuda" clang-format)
                                  ("D" dfmt)
                                  ("Dart" dart-format)
                                  ("Dhall" dhall)
                                  ("Dockerfile" dockfmt)
                                  ("Elixir" mix-format)
                                  ("Elm" elm-format)
                                  ("Emacs Lisp" emacs-lisp)
                                  ("Erlang" efmt)
                                  ("F#" fantomas)
                                  ("Fish" fish-indent)
                                  ("Fortran Free Form" fprettify)
                                  ("GLSL" clang-format)
                                  ("Go" gofmt)
                                  ("GraphQL" prettier)
                                  ("Haskell" brittany)
                                  ("HTML" html-tidy)
                                  ("HTML+ERB" erb-format)
                                  ("Java" clang-format)
                                  ("JavaScript" prettier)
                                  ("JSON" prettier)
                                  ("JSON5" prettier)
                                  ("Jsonnet" jsonnetfmt)
                                  ("JSX" prettier)
                                  ("Kotlin" ktlint)
                                  ("LaTeX" latexindent)
                                  ("Less" prettier)
                                  ("Literate Haskell" brittany)
                                  ("Lua" stylua)
                                  ("Markdown" prettier)
                                  ("Nix" nixpkgs-fmt)
                                  ("Objective-C" clang-format)
                                  ("OCaml" ocp-indent)
                                  ("Perl" perltidy)
                                  ("PHP" prettier)
                                  ("Protocol Buffer" clang-format)
                                  ("PureScript" purty)
                                  ("Python" black)
                                  ("R" styler)
                                  ("Reason" bsrefmt)
                                  ("ReScript" rescript)
                                  ("Ruby" rufo)
                                  ("Rust" rustfmt)
                                  ("Scala" scalafmt)
                                  ("SCSS" prettier)
                                  ("Shell" shfmt)
                                  ("Solidity" prettier)
                                  ("SQL" sqlformat)
                                  ("Svelte" prettier)
                                  ("Swift" swiftformat)
                                  ("Terraform" terraform-fmt)
                                  ("TOML" prettier)
                                  ("TSX" prettier)
                                  ("TypeScript" prettier)
                                  ("V" v-fmt)
                                  ("Verilog" istyle-verilog)
                                  ("Vue" prettier)
                                  ("XML" html-tidy)
                                  ("YAML" prettier)
                                  )
                                 )
  )

;; EditorConfig support
(leaf
  editorconfig
  :straight t
  :require t
  :blackout t
  :global-minor-mode editorconfig-mode)

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
   (fussy-filter-fn . 'fussy-filter-default)
   (fussy-score-fn . 'fussy-fuz-bin-score)
   (fussy-fuz-use-skim-p . t))
  :config
  ;; integrate with company
  (defun j-company-capf (f &rest args)
    "Manage `completion-styles'."
    (let ((fussy-max-candidate-limit 5000)
          (fussy-default-regex-fn 'fussy-pattern-first-letter)
          (fussy-prefer-prefix nil))
      (apply f args)))

  (defun j-company-transformers (f &rest args)
    "Manage `company-transformers'."
    (let ((company-transformers '(fussy-company-sort-by-completion-score)))
      (apply f args)))

  (advice-add 'company--transform-candidates :around 'j-company-transformers)
  (advice-add 'company-capf :around 'j-company-capf)
  )

(leaf company
  :straight t
  :require t
  :bind
  (([remap indent-for-tab-command] . company-indent-or-complete-common)
   ([remap c-indent-line-or-region] . company-indent-or-complete-common)
   (:company-active-map
    ("C-w" . nil)
    ))
  :global-minor-mode global-company-mode
  :custom
  (
   (company-idle-delay . 0.1)
   (company-minimum-prefix-length . 2)
   (company-tooltip-align-annotations . t)
   (company-dabbrev-other-buffers . t)
   (company-dabbrev-ignore-case . nil)
   (company-dabbrev-downcase . nil)
   (company-require-match . 'never)
   (company-transformers . '(delete-consecutive-dups company-sort-by-backend-importance))
   (company-auto-complete . nil))
  :config
  (add-to-list 'company-frontends 'company-pseudo-tooltip-frontend t)
  ;; (add-to-list 'company-frontends 'company-echo-metadata-frontend t)
  (setq company-backends '(
                           ;; get lsp completion first when available
                           (comany-capf
                            :with
                            company-keywords
                            company-yasnippet
                            company-dabbrev
                            ;; company-dabbrev-code
                            company-same-mode-buffers
                            company-semantic
                            company-gtags
                            company-etags
                            company-oddmuse
                            company-bbdb
                            )
                           (company-keywords
                            company-yasnippet
                            company-dabbrev
                            ;; company-dabbrev-code
                            company-same-mode-buffers
                            company-semantic
                            company-gtags
                            company-etags
                            company-oddmuse
                            company-bbdb
                            )
                           company-files
                           company-dabbrev
                           company-wordfreq
                           company-bbdb
                           company-oddmuse
                           company-same-mode-buffers
                           ))
  )

(leaf company-box
  :straight t
  :require t
  :after company
  :hook (company-mode-hook . company-box-mode)
  )

(leaf company-dwim
  :straight (company-dwim :type git :host github :repo "zk-phi/company-dwim")
  :require t
  :after company
  :bind (:company-active-map
         ("TAB" . company-dwim)
         ("<tab>" . company-dwim)
         ("S-TAB" . company-dwim-select-previous)
         ("<backtab>" . company-dwim-select-previous)
         ("C-j" . company-complete-selection))
  :config
  (add-to-list 'company-frontends 'company-dwim-frontend t))

(leaf company-anywhere
  :straight (company-anywhere :type git :host github :repo "zk-phi/company-anywhere")
  :require t)

(leaf company-same-mode-buffers
  :straight (company-same-mode-buffers :type git :host github :repo "zk-phi/company-same-mode-buffers")
  :require t
  :after company
  :custom
  (company-same-mode-buffers-matchers .
                                      '(
                                        ;; company-same-mode-buffers-matcher-partial
                                        ;; company-same-mode-buffers-matcher-exact-first-letter-flex-rest
                                        company-same-mode-buffers-matcher-flex
                                        ))
  :config
  (company-same-mode-buffers-initialize)
  )

(leaf company-wordfreq
  :straight t
  :require t
  :after company
  )

(leaf
  yasnippet
  :straight t
  :require t
  :blackout t
  :bind
  (:yas-keymap
   ("<tab>" . nil)
   ("TAB" . nil)
   ("<backtab>" . nil)
   ("S-TAB" . nil)
   ("C-o" . yas-next-field-or-maybe-expand))
  :global-minor-mode yas-global-mode)

(leaf yasnippet-snippets
  :straight t
  :require t
  :after yasnippet)

(leaf
  vertico
  :straight t
  :require t
  :global-minor-mode vertico-mode
  :custom ((vertico-cycle . t)))

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
      (
       consult--source-project-buffer
       consult--source-buffer
       consult--source-hidden-buffer
       consult--source-bookmark
       consult--source-recent-file)))
    :config
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-recent-file
     consult--source-project-recent-file
     :preview-key '(:debounce 0.2 any))
    :bind
    ;; ((:evil-normal-state-map
    ;;   ;; ("C-f" . consult-line)
    ;;   ("C-f" . (lambda () (interactive)(if (switch-to-minibuffer) nil (consult-line))))
    ;;   ("<leader>SPC" . 'consult-buffer)
    ;;   ))
    )

  ;; flycheck integration
  (leaf consult-flycheck
    :straight t
    :require t
    :after consult flycheck
    :bind
    (
;; (:evil-normal-state-map
;;       ("<leader>q" . (lambda () (interactive)(if (switch-to-minibuffer) nil (consult-flycheck))))
;;       )
))

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
    ("C-x C-j" . consult-dir-jump-file))
   ;; (:evil-normal-state-map
   ;;  ("C-S-r" . consult-dir)
   ;;  )
   )
  :custom
  (consult-dir-project-list-function . #'consult-dir-projectile-dirs))

;;; embark
(leaf embark :straight t :require t
  :doc "minibuffer actions"
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  )

;; consult integration
(leaf embark-consult
  :straight t
  :require t)

;; marginalia
(leaf
  marginalia
  :straight t
  :require t
  :defvar (marginalia-mode)
  :global-minor-mode marginalia-mode)

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

(leaf
  savehist
  :straight t
  :require t
  :defvar (savehist-coding-system)
  :init (savehist-mode)
  :setq (savehist-coding-system . 'utf-8-emacs))

(leaf switch-to-minibuffer
  :init
  (defun switch-to-minibuffer ()
    "Switch to minibuffer window."
    (interactive)
    (if (active-minibuffer-window)
        (select-window (active-minibuffer-window))
      nil))
  )

;; org mode things
(leaf org
  :straight (org :type built-in)
  :require t org-tempo
  :custom
  (org-catch-invisible-edits . 'smart)
  :config
  (defun disable-flycheck-in-org-src-block ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'org-src-mode-hook 'disable-flycheck-in-org-src-block)
  ;; keymap for meow modal
  (when (featurep 'meow)
    (bind-keys :map org-mode-map
               ("C-S-<return>" . org-edit-special)
               ("C-j" . org-next-visible-heading)
               ("C-k" . org-previous-visible-heading)
               ("C-S-j" . org-move-subtree-down)
               ("C-S-k" . org-move-subtree-up))
    )
  )

(leaf org-bullets
  :straight t
  :require t
  :after org
  :hook (org-mode-hook . org-bullets-mode)
  :custom
  (org-bullets-bullet-list . '("◉" "○" "●" "○" "●" "○" "●")))

(leaf
  org-appear
  :require t
  :straight
  (org-appear
   :type git
   :host github
   :repo "awth13/org-appear")
  :hook
  (org-mode-hook . org-appear-mode)
  :after org-mode
  :custom
  (org-appear-autolinks . t)
  (org-appear-autosubmarkers . t)
  (org-appear-autoentities . t)
  (org-appear-autokeywords . t)
  (org-appear-inside-latex . t))

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
  ;; (lsp-completion-mode . my/lsp-mode-setup-completion)
  (c++-mode-hook . lsp)
  :custom
  ((lsp-idle-delay . 0.5)
   (lsp-log-io . t)
   (lsp-completion-provider . :capf)
   (lsp-keymap-prefix . "C-c l")))

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

(leaf lua-mode
  :straight t
  :require t
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
