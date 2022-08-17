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

(setq straight-repository-branch "develop"
      ;; don't check on find-at-startup
      straight-check-for-modifications '(check-on-save find-when-checking))

;; setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; install leaf requirements by straight
(straight-use-package 'leaf) (straight-use-package 'leaf-keywords)

(leaf leaf-keywords
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
  (leaf-keywords-init))

;; leaf plugins
(leaf leaf
  :config
  (leaf leaf-convert :straight t)
  )

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
   (backup-directory-alist . '((".*" . "~/.backup")))
   (create-lockfiles . nil)
   (debug-on-error . nil)
   (init-file-debug . nil)
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
   (show-paren-style . 'parenthesis)
   (bookmark-watch-bookmark-file . 'silent))
  :init
  (defalias 'yes-or-no-p 'y-or-n-p)
  (define-key key-translation-map [?\C-h] [?\C-?])
  (global-set-key (kbd "C-?") 'help-for-help)
  (define-key input-decode-map [?\C-i] [C-i])
  )

(leaf
  cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag
  "builtin"
  "faces"
  "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf
  persistent-scratch
  :doc "keep scratch buffer state across sessions"
  :straight t
  :defun (persistent-scratch-setup-default)
  :config (persistent-scratch-setup-default))

(leaf gcmh
  :straight t
  :blackout t
  :defun (gcmh-mode)
  :hook (windows-startup-hook . gcmh-mode)
  :custom
  (gcmh-verbose . t)
  :config
  (setq gcmh-idle-delay 'auto  ; default is 15s
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  )

;; explain macro by step
(leaf
  macrostep
  :straight t
  :require t
  :bind (("C-c e" . macrostep-expand)))

(leaf restart-emacs
  :straight t
  :commands (restart-emacs restart-emacs-start-new-emacs))

(leaf exec-path-from-shell
  :straight t
  :config
  ;; setup on mac/linux
  (when (or IS-MAC IS-LINUX)
    ;; (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    ;; add mason's executable to emacs's exec-path
    (let ((mason-path (expand-file-name "~/.local/share/nvim/mason/bin/")))
      (setq exec-path (add-to-list 'exec-path mason-path)))
    )
  )

;; WSL-specific setup
(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))

  ;; Teach Emacs how to open links in your default Windows browser
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic))))

;; mozc ime
(leaf
  mozc
  ;; :depends emacs-mozc
  :straight t
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

(set-face-attribute 'default nil :font "Iosevka Term-14")
(set-face-attribute 'fixed-pitch nil :family "Iosevka Term" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "MigMix 1M" :height 1.0)

;; Japanese font
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "Migmix 1M"))

;; icons dependency
(leaf all-the-icons
  :straight t
  :custom
  (all-the-icons-scale-factor . 1.1)
  )

(leaf
  kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf emojify
  :straight t
  :hook (after-init-hook . global-emojify-mode))

(leaf doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-molokai t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(leaf dashboard
  :straight t
  :hook (after-init-hook . dashboard-setup-startup-hook)
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
                       (bookmarks . 10)
                       (projects . 5)))
  :defer-config
  (setq dashboard-footer-icon (all-the-icons-material "dashboard"
                                                      :height 1.0
                                                      :v-adjust -0.05
                                                      :face 'font-lock-keyword-face))
  )

(leaf doom-modeline
  :straight t
  :hook
  (window-setup-hook . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes . nil)
  (doom-modeline-major-mode-icon . t)
  (doom-modeline-bar-width . 6)
  (doom-modeline-height . 15)
  (doom-modeline-modal-icon . nil)
  (doom-modeline-icon . t)
  :defer-config
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

(leaf centaur-tabs
  :straight t
  :require t
  :hook
  ;; disable on specific modes
  (dired-mode-hook . centaur-tabs-local-mode)
  (dirvish-mode-hook . centaur-tabs-local-mode)
  (backtrace-mode-hook . centaur-tabs-local-mode)
  (eshell-mode-hook . centaur-tabs-local-mode)
  :defer-config
  (centaur-tabs-mode 1)
  (centaur-tabs-group-by-projectile-project)
  :custom
  (centaur-tabs-style . "bar")
  (centaur-tabs-set-icons . t)
  (centaur-tabs-cycle-scope . 'tabs)
  :bind
  ("C-<tab>" . centaur-tabs-forward)
  ("C-<iso-lefttab>" . centaur-tabs-backward)
  )

(leaf dirvish
  :straight t
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
    ("l" . dired-find-file)
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

(leaf git-modes
  :straight t)

(leaf
  magit
  :doc "great git client"
  :straight t
  :bind
  (:magit-status-mode-map
   ("x" . magit-delete-thing))
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
     ("c" magit-commit "commit" :color blue)))))

(leaf
  forge
  :doc "remote repo control with magit"
  :straight t
  :after magit
  :custom
  ((bug-reference-mode . 0) (forge-add-default-bindings . nil)))

(leaf
  magit-todos
  :doc "manage TODO keywords with magit"
  :straight t
  :after magit
  :hook (magit-mode-hook . magit-todos-mode))

(leaf
  diff-hl
  :straight t
  :hook
  ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :global-minor-mode global-diff-hl-mode
  :custom
  (diff-hl-show-staged-changes . nil)
  (diff-hl-ask-before-revert-hunk . t))

(leaf git-timemachine
  :straight t
  :commands (git-timemachine git-timemachine-toggle))

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
    "~/.emacs.d/straight/.*"
    (lambda (file) (file-in-directory-p file package-user-dir)))))

(leaf saveplace
  :config
  (save-place-mode 1))

;; project management
(leaf
  projectile
  :straight t
  :bind (:projectile-mode-map ("C-c p" . projectile-command-map))
  :global-minor-mode t)

(leaf perspective
  :straight t
  :hook
  ;; (kill-emacs-hook . persp-state-save)
  :custom
  ;; (persp-state-default-file . "~/.emacs.d/persp-state-file")
  (persp-suppress-no-prefix-key-warning . t)
  :init
  (customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p")) ; HACK: kbd doesn't work on :custom
  (persp-mode)
  )

(leaf persp-projectile
  :straight t)

;; sessions as bookmark
(leaf burly
  :straight
  (burly
   :type git
   :host github
   :repo "alphapapa/burly.el")
  :config
  ;; save current windows with project name
  (defun burly-perspective-init-project-persp nil
    "Save burly windows bookmark with current buffer's project name, then set persp's name to project one."
    (interactive)
    (let ((project-name (projectile-project-name projectile-project-root))
          (persp-name (persp-current-name)))

      ;; override project-name if it's "-" (saved with *scratch*)
      (when (equal project-name "-")
        (setq project-name persp-name))
      ;; rename perspective when current persp is not project's name
      (when (not (equal project-name persp-name))
        ;; kill existing project's perspective before rename
        (when (gethash project-name (perspectives-hash))
          (persp-kill project-name)
          )
        (persp-rename project-name)
        ;; ;; create perspective with last perspective name
        ;; (persp-new persp-name)
        )

      ;; ;; ask persp-name interactively if it's "main"
      ;; (when (and (equal persp-name persp-initial-frame-name)
      ;;            ;; NOTE: shouldn't ask when going from *dashboard* to any burly bookmark
      ;;            ;; FIXME: buffer-name doesn't work in this case
      ;;            (string-match "\*dashboard\*" (buffer-name)))
      ;;   (call-interactively 'persp-rename)
      ;;   (setq project-name persp-name)
      ;;   )

      ;; save as burly-bookmark, don't save "main" persp
      (unless (equal persp-name persp-initial-frame-name)
        (burly-bookmark-windows project-name))
      )
    )

  ;; initalize only if in "main" persp
  (defun burly-perspective-init-if-initial-frame (&rest _ignore)
    (when (equal persp-initial-frame-name (persp-current-name))
      (burly-perspective-init-project-persp)
      )
    )
  ;; (add-hook 'find-file-hooks 'burly-perspective-init-if-initial-frame)
  ;; (advice-add 'find-file-noselect :after 'burly-perspective-init-if-initial-frame)


  ;; save current windows with perspective name
  (defun burly-bookmark-perspective-windows (&rest _ignore)
    "Save burly windows bookmark with current perspective name."
    (interactive)
    ;; save non-"main" perspective
    (when (not (equal persp-initial-frame-name (persp-current-name)))
      (burly-bookmark-windows (persp-current-name)))
    ;; save "main" perspective as project one
    (when (equal persp-initial-frame-name (persp-current-name))
      (burly-perspective-init-project-persp)
      )
    )

  ;; save perspective on change/save
  (add-hook 'persp-before-switch-hook 'burly-bookmark-perspective-windows)
  ;; (add-hook 'auto-save-hook 'burly-bookmark-perspective-windows)
  (add-hook 'before-save-hook 'burly-bookmark-perspective-windows)
  ;; (add-hook 'window-configuration-change-hook 'burly-bookmark-perspective-windows)
  (advice-add 'save-buffers-kill-emacs :before 'burly-bookmark-perspective-windows)

  ;; create perspective with burly
  (defun burly-perspective--windows-set-before-advice (&rest _ignore)
    "Create or Switch perspective before setting burly windows."
    (persp-switch burly-opened-bookmark-name)
    )
  (advice-add #'burly--windows-set :before #'burly-perspective--windows-set-before-advice)
  )

;; my utility keymap
(leaf kurumi-utility
  :config
  (defvar kurumi-utility-map)
  (setq kurumi-utility-map (make-sparse-keymap))
  (bind-keys :map kurumi-utility-map
             ("g d" . xref-find-definitions)
             ("g r" . xref-find-references)
             ("c a" . lsp-execute-code-action)
             ("r n" . lsp-rename)
             ("q" . flycheck-list-errors)
             ))

(leaf
  which-key
  :doc "which-key in emacs"
  :straight t
  :defun (which-key-setup-side-window-right which-key-mode)
  :blackout which-key-mode
  :custom (which-key-idle-delay . 0.25)
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

(leaf
  display-line-numbers
  :bind ("<f9>" . display-line-numbers-mode)
  :hook ((prog-mode-hook text-mode-hook) . display-line-numbers-mode)
  :custom (display-line-numbers-width . 3))

(leaf hl-line :global-minor-mode global-hl-line-mode)

(leaf hl-block-mode
  :doc "blockman thing"
  :straight t
  :hook
  (prog-mode-hook . global-hl-block-mode)
  :custom
  (hl-block-delay . 0.1)
  (hl-block-bracket . nil)
  (hl-block-multi-line . nil)
  (hl-block-color-tint . "#060606")
  )

(leaf
  hl-todo
  :doc "TODO keywords highlighting"
  :straight t
  :global-minor-mode global-hl-todo-mode)

(leaf
  highlight-indent-guides
  :doc "indent lines"
  :straight t
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
  :hook ((prog-mode-hook org-mode-hook) . rainbow-delimiters-mode))

(leaf rainbow-mode
  :straight t
  :blackout t
  :hook
  (prog-mode-hook . rainbow-mode)
  (org-mode-hook . rainbow-mode))

;; focus window
(leaf
  zoom
  :straight t
  :blackout t
  :global-minor-mode zoom-mode
  :custom (zoom-size . '(0.618 . 0.618)))

(leaf centered-cursor-mode
  :straight t
  :blackout t
  :global-minor-mode global-centered-cursor-mode
  :custom (ccm-step-size . 2)
  :config
  ;; exclude on vterm
  (add-to-list 'ccm-ignored-commands 'vterm--self-insert))

;; enhanced help
(leaf helpful
  :straight t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h k" . helpful-key)
  ("C-h v" . helpful-variable))

(leaf winner-mode
  :bind
  (("C-z" . winner-undo)
   ("C-S-z" . winner-redo))
  :global-minor-mode winner-mode
  )

(leaf move-or-create-window
  :doc "focus.nvim in emacs"
  :config
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

(leaf meow
  :straight t
  :require t
  :init
  ;; key-chord dependency
  (leaf key-chord
    :straight t
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
  (defun meow-close-window-or-buffer ()
    (interactive)
    (if (> (seq-length (window-list (selected-frame))) 1)
        (delete-window)
      (kill-this-buffer)))

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
     '("C-w" . kill-this-buffer)
     '("/" . consult-line)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     '("C-u" . "H-C-u")
     '("C-d" . "H-C-d")
     '("C-o" . "H-C-o")
     '("<C-i>" . "H-C-i")
     '("C-w" . "H-C-w")
     '("/" . "H-/")
     ;; move-or-create-window prefix
     '("w j" . move-or-create-window-below)
     '("w k" . move-or-create-window-above)
     '("w h" . move-or-create-window-left)
     '("w l" . move-or-create-window-right)
     ;; version control operations
     '("v" . my-git-actions/body)
     ;; spell correction
     '("z g" . flyspell-correct-at-point)
     ;; lsp-command-map
     '("l" . "s-l")
     ;; perspective
     '("p" . "C-c M-p")
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
     ;; '("D" . meow-backward-delete)
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
     ;; '("r" . embrace-commander)
     ;; '("R" . meow-swap-grab)
     '("R" . repeat)
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
     '("\"" . consult-yank-pop)
     '("/" . consult-line)
     '("C-u" . ccm-scroll-down)
     '("C-d" . ccm-scroll-up)
     '("C-o" . my/backward-forward-previous-location)
     '("<C-i>" . my/backward-forward-next-location)
     ;; '("C-o" . gumshoe-persp-backtrack-back)
     ;; '("<C-i>" . gumshoe-persp-backtrack-forward)
     '("C-f" . consult-line)
     ;; '("C-p" . affe-find)
     ;; '("C-e" . find-file)
     '("C-t" . burly-perspective-init-project-persp)
     '("C-s" . save-buffer)
     '("C-w" . kill-this-buffer)
     ;; '("TAB" . origami-toggle-node)
     ;; '("<tab>" . origami-toggle-node)
     (cons "S-SPC" kurumi-utility-map)
     '("C-s" . save-buffer)
     '("<escape>" . ignore))
    ;; insert state keymap
    (meow-define-keys
        'insert
      '("C-g" . meow-insert-exit)
      )
    ;; readline-style keymap in global map
    (bind-key "C-w" 'backward-kill-word)
    ;; remap universal-argument
    (bind-key "C-M-u" 'universal-argument)
    ;; key-chord shortcuts
    (key-chord-define meow-insert-state-keymap "jk" 'meow-insert-exit)
    (key-chord-define meow-normal-state-keymap "gd" 'xref-find-definitions)
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
                            (help-mode . normal)
                            (Man-mode . normal)
                            (vterm-mode . insert)
                            (eshell-mode . insert)
                            ))
  (meow--kbd-forward-char . "<right>")
  ;; (meow--kbd-forward-line . "<down>")
  ;; (meow--kbd-backward-line . "<up>")
  (meow--kbd-delete-char . "<deletechar>")
  (meow--kbd-kill-region . "S-<delete>")
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
                           (?\( . round)
                           (?b . anyblock) ;; `b' for bracket
                           (?c . curly)
                           (?s . string)
                           (?e . symbol)
                           (?w . window)
                           (?B . buffer)
                           (?p . paragraph)
                           (?\[ . square)
                           (?l . line)
                           (?d . defun)
                           (?. . sentence)))
  :defer-config
  (meow-setup)
  ;; vim-way cursor
  ;; Must set before enable `meow-global-mode`
  ;; (setq meow-use-cursor-position-hack t
  ;;       meow-use-enhanced-selection-effect t)  ;; optional, for visual effect
  (meow-global-mode 1)
  )

(leaf expand-region
    :straight t)

(leaf
  smartparens
  :straight t
  :require smartparens-config
  :blackout t
  :global-minor-mode smartparens-global-mode)

(leaf embrace
  :straight t)

(leaf smart-hungry-delete
  :straight t
  :bind
  (("<deletechar>" . 'smart-hungry-delete-backward-char)
   ("<delete>" . 'smart-hungry-delete-backward-char)
   ("C-d" . 'smart-hungry-delete-forward-char)
   ([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
   ([remap delete-backward-char] . smart-hungry-delete-backward-char)
   ([remap delete-char] . smart-hungry-delete-forward-char))
  :init
  (smart-hungry-delete-add-default-hooks))

(leaf backward-forward
  :straight t
  :global-minor-mode t
  :custom
  (backward-forward-mark-ring-max . 100)
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
  )

;; undo
(leaf undo-fu
  :straight t
  :bind
  ([remap undo] . undo-fu-only-undo)
  ([remap redo] . undo-fu-only-redo)
  )
(leaf
  undo-fu-session
  :straight t
  :global-minor-mode global-undo-fu-session-mode)

;; Code folding
(leaf
  origami
  :straight t
  ;; :after evil
  :global-minor-mode global-origami-mode)

(leaf avy
  :straight t
  :custom
  (avy-timeout-seconds . 0.3)
  (avy-orders-alist .
    '((avy-goto-char-0 . avy-order-closest)
      (avy-goto-word-0 . avy-order-closest)))
  )

(leaf dumb-jump
  :straight t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; flycheck syntax checking
(leaf
  flycheck
  :straight t
  :require t
  :global-minor-mode global-flycheck-mode
  :init
  (leaf flycheck-inline
    :straight t
    :hook (flycheck-mode-hook . flycheck-inline-mode))
  :custom
  (flycheck-display-errors-delay . 0.1)
  :config
  ;; tweak fringe icons
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00111000
            #b01111100
            #b11111110
            #b11111110
            #b01111100
            #b00111000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning
    )

  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info
    )
  )

(leaf flyspell
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

(leaf flyspell-lazy
  :straight
  (flyspell-lazy
  :type git
  :host github
  :repo "rolandwalker/flyspell-lazy")
  :after flyspell
  :custom
  (flyspell-lazy-idle-seconds . 1)
  (flyspell-lazy-window-idle-seconds . 3)
  :config
  (flyspell-lazy-mode)
  )

(leaf
  flyspell-correct
  :straight t
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
(leaf editorconfig
  :straight t
  :require t
  :blackout t
  :global-minor-mode editorconfig-mode)

(leaf dtrt-indent
  :straight t
  :hook
  (prog-mode-hook . dtrt-indent-mode)
  :config
  ;; (add-hook 'prog-mode-hook #'(lambda ()
  ;;                               (dtrt-indent-mode)
  ;;                               (dtrt-indent-adapt)))
  )

(leaf company
  :straight t
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
   (company-selection-wrap-around . t)
   (company-tooltip-align-annotations . t)
   (company-dabbrev-other-buffers . 'all)
   (company-dabbrev-ignore-case . nil)
   (company-dabbrev-downcase . nil)
   (company-dabbrev-code-everywhere . t)     ;;  include comments and strings.
   (company-require-match . 'never)
   (company-transformers . '(delete-consecutive-dups company-sort-by-backend-importance))
   (company-auto-complete . nil))
  :config
  (add-to-list 'company-frontends 'company-pseudo-tooltip-frontend t)
  ;; (add-to-list 'company-frontends 'company-echo-metadata-frontend t)
  (setq company-backends '(
                           ;; get lsp completion first when available
                           (company-capf
                            :with
                            company-keywords
                            company-yasnippet
                            ;; company-dabbrev
                            company-dabbrev-code
                            company-same-mode-buffers
                            company-semantic
                            company-gtags
                            company-etags
                            company-oddmuse
                            company-bbdb
                            )
                           (company-keywords
                            company-yasnippet
                            ;; company-dabbrev
                            company-dabbrev-code
                            company-same-mode-buffers
                            company-semantic
                            company-gtags
                            company-etags
                            company-oddmuse
                            company-bbdb
                            )
                           company-files
                           company-dabbrev
                           company-same-mode-buffers
                           company-wordfreq
                           company-bbdb
                           company-oddmuse
                           ))
  ;; magit commit message completion with magit-diff
  (defun my--company-dabbrev-ignore-except-magit-diff (buffer)
    (let ((name (buffer-name)))
      (and (string-match-p "\\`[ *]" name)
           (not (string-match-p "\\*magit-diff:" name)))))

  (defun my--git-commit-setup-hook ()
    (setq-local fill-column 72)

    (setq-local company-dabbrev-code-modes '(text-mode magit-diff-mode))
    (setq-local company-dabbrev-ignore-buffers
                #'my--company-dabbrev-ignore-except-magit-diff))

  (add-hook 'git-commit-setup-hook #'my--git-commit-setup-hook)
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
                                        company-same-mode-buffers-matcher-flex
                                        company-same-mode-buffers-matcher-partial
                                        company-same-mode-buffers-matcher-exact-first-letter-flex-rest
                                        ))
  :config
  (company-same-mode-buffers-initialize)
  )

(leaf company-wordfreq
  :straight t
  :require t
  :after company
  )

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

(leaf
  yasnippet
  :straight t
  :require t
  :blackout t
  :global-minor-mode yas-global-mode)

(leaf yasnippet-snippets
  :straight t
  :require t
  :after yasnippet)

(leaf
  vertico
  :straight t
  :global-minor-mode vertico-mode
  :custom ((vertico-cycle . t))
  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  )

(leaf consult
  :straight t
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
     consult-projectile--source-projectile-buffer
     persp-consult-source
     consult--source-bookmark
     consult-projectile--source-projectile-project
     consult-projectile--source-projectile-recentf
     consult--source-buffer
     consult--source-hidden-buffer
     consult--source-recent-file
     )))
  :config
  ;; don't set sources on top
  (consult-customize consult--source-buffer persp-consult-source :default nil)
  ;; hide outer-perspecitve sources
  (consult-customize consult--source-buffer consult--source-recent-file :hidden t)
  ;; preview delay
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key '(:debounce 0.3 any))
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

(leaf consult-projectile
  :straight (consult-projectile
             :type git
             :host gitlab
             :repo "OlMon/consult-projectile"
             )
  :require t
  :custom
  ;; (consult-projectile-use-projectile-switch-project . t)
  (consult-projectile-source-projectile-project-action . 'kurumi-consult-projectile--project-persp-action) ;; hook my custom action
  :config
  ;; create persp and cd before consult-projectile
  (defun kurumi-consult-projectile--project-persp-action (dir)
    (persp-switch (projectile-project-name dir))
    (cd (projectile-project-root dir))
    (consult-projectile--file dir)
    )
  )

(leaf consult-flyspell
  :straight (consult-flyspell :type git :host gitlab :repo "OlMon/consult-flyspell" :branch "master")
  :after (consult flyspell)
  :config
  (setq consult-flyspell-select-function 'flyspell-correct-at-point
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer t)
  )

;;; embark
(leaf embark
  :straight t
  :doc "minibuffer actions"
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  )

;; consult integration
(leaf embark-consult
  :straight t
  :after (embark consult))

;; marginalia
(leaf
  marginalia
  :straight t
  :defvar (marginalia-mode)
  :global-minor-mode marginalia-mode)

;; affe fuzzy-finder
(leaf
  affe
  :straight t
  :commands (affe-grep affe-find)
  :init
  (leaf orderless :straight t :commands (orderless-pattern-compiler))
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(leaf
  savehist
  :straight t
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

(leaf go-translate
  :straight t
  :require t
  :config
  (setq gts-translate-list '(("en" "ja") ("ja" "en")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list
                   (gts-bing-engine)
                   (gts-google-engine)
                   (gts-deepl-engine :auth-key (getenv "DEEPL_TOKEN") :pro nil))
         :render (gts-buffer-render)))
  )

(leaf hyperbole
  :straight t
  :hook
  ;; (window-setup-hook . hyperbole-mode)
  (text-mode-hook . hyperbole-mode)
  (prog-mode-hook . hyperbole-mode)
  (conf-mode-hook . hyperbole-mode)
  (yaml-mode-hook . hyperbole-mode)
  )

(leaf tree-sitter
  :straight t
  :global-minor-mode global-tree-sitter-mode
  :hook
  (treesitter-after-on-hook . tree-sitter-hl-mode))

(leaf tree-sitter-langs
  :straight t
  :after tree-sitter)

(leaf vterm
  :straight t
  :ensure-system-package cmake libtool (libtool . libtool-bin)
  :init
  (leaf vterm-toggle
    :straight t)
  :bind
  (
   ;; ("C-@" . vterm-toggle)
   (:vterm-mode-map
    ("<C-w>" . (lambda () (interactive) (vterm-send-key (kbd "C-w"))))
    ))
  :custom
  (vterm-max-scrollback . 10000)
  )

(leaf eshell
  :require t
  :custom
  (eshell-cmpl-ignore-case . t)
  (eshell-hist-ignoredups . t)
  (eshell-scroll-to-bottom-on-input . 'all)
  (eshell-scroll-to-bottom-on-output . 'all)
  (eshell-kill-processes-on-exit . t)
  :defer-config
  ;; HACK: can't map directly to eshell-mode-map
  (add-hook 'eshell-mode-hook
            (lambda ()
              (progn
                ;; avoid overriding c-n/c-p for meow compatibility
                (define-key eshell-mode-map "\C-k" 'eshell-previous-matching-input-from-input)
                (define-key eshell-mode-map "\C-j" 'eshell-next-matching-input-from-input))))
  )

(leaf eshell-toggle
  :straight t
  :require t
  :custom
  (eshell-toggle-size-fraction . 3)
  (eshell-toggle-use-projectile-root . nil)
  (eshell-toggle-run-command . nil)
  (eshell-toggle-init-function . #'eshell-toggle-init-eshell)
  ;; (eshell-toggle-init-function . #'eshell-toggle-init-ansi-term)
  :bind
  ("C-@" . eshell-toggle))

(leaf eshell-up
  :straight t
  :require t
  )

(leaf esh-autosuggest
  :straight t
  :hook (eshell-mode-hook . esh-autosuggest-mode)
  )

(leaf fish-completion
  :unless IS-WINDOWS
  :straight t
  :hook (eshell-mode-hook . fish-completion-mode)
  :custom
  (fish-completion-fallback-on-bash-p . t)
  :config
  ;; HACK Even with `fish-completion-fallback-on-bash-p' non-nil,
  ;;      `fish-completion--list-completions-with-desc' will throw an error if
  ;;      fish isn't installed (and so, will fail to fall back to bash), so we
  ;;      advise it to fail silently.
  (defun +eshell--fallback-to-bash-a (&rest _)
    (unless (executable-find "fish") ""))
  (advice-add '+eshell--fallback-to-bash-a :before #'fish-completion--list-completions-with-desc)
  )

(leaf eshell-syntax-highlighting
  :straight t
  :hook (eshell-mode-hook . eshell-syntax-highlighting-global-mode)
  )

(leaf eshell-did-you-mean
  :straight t
  :after eshell-mode ;; not eshell
  :config
  (eshell-did-you-mean-setup)
  ;; HACK There is a known issue with `eshell-did-you-mean' where it does not
  ;;      work on first invocation, so we invoke it once manually by setting the
  ;;      last command and then calling the output filter.
  (setq eshell-last-command-name "catt")
  (eshell-did-you-mean-output-filter "catt: command not found")
  )

(leaf eshell-prompt-extras
  :straight t
  :commands (epe-theme-lambda)
  :custom
  (eshell-highlight-prompt . nil)
  (eshell-prompt-function . #'epe-theme-lambda)
  )

(leaf eshell-vterm
  :straight t
  ;; :after eshell
  :config
  (eshell-vterm-mode)
  (defalias 'eshell/v 'eshell-exec-visual)
  )

(leaf friendly-shell
  :straight t)

(leaf friendly-shell-command
  :straight t)

(leaf friendly-remote-shell
  :straight t)

(leaf parinfer-rust-mode
  :straight t
  :hook (emacs-lisp-mode-hook . parinfer-rust-mode)
  :custom
  (parinfer-rust-auto-download . t))

;; org mode things
(leaf org
  :straight (org :type built-in)
  :require org-tempo
  :custom
  (org-catch-invisible-edits . 'smart)
  (org-src-window-setup . 'split-window-right)
  :defer-config
  (defun disable-flycheck-in-org-src-block ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (add-hook 'org-src-mode-hook 'disable-flycheck-in-org-src-block)
  ;; keymap for meow modal
  (when (featurep 'meow)
    (bind-keys :map org-mode-map
               ("C-S-e" . org-edit-special)
               ("C-j" . org-next-visible-heading)
               ("C-k" . org-previous-visible-heading)
               ("C-S-j" . org-move-subtree-down)
               ("C-S-k" . org-move-subtree-up))
    )
  )

(leaf org-auto-tangle
  :straight t
  :after org
  :hook
  (org-mode-hook . org-auto-tangle-mode)
  :custom
  (org-auto-tangle-default . t))

(leaf org-bullets
  :straight t
  :after org
  :hook (org-mode-hook . org-bullets-mode)
  :custom
  (org-bullets-bullet-list . '("◉" "○" "●" "○" "●" "○" "●")))

(leaf
  org-appear
  :straight
  (org-appear
   :type git
   :host github
   :repo "awth13/org-appear")
  :after org
  :hook
  (org-mode-hook . org-appear-mode)
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
  ;; (lsp-mode . lsp-enable-which-key-integration)
  ;; (lsp-completion-mode . my/lsp-mode-setup-completion)
  (c++-mode-hook . lsp-deferred)
  :custom
  (lsp-idle-delay . 0.5)
  (lsp-log-io . t)
  (lsp-auto-guess-root . t)
  (lsp-completion-provider . :capf)
  (lsp-keymap-prefix . "s-l"))

(leaf lsp-ui
  :doc "UI modules for lsp-mode"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :straight t
  :after lsp-mode
  :bind
  (:lsp-ui-mode-map
   ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
   ([remap xref-find-references] . lsp-ui-peek-find-references))

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
   (lsp-ui-sideline-show-diagnostics . nil) ;; use flycheck-inline
   (lsp-ui-sideline-show-hover . nil)
   (lsp-ui-sideline-show-code-actions . t)
   (lsp-ui-flycheck-enable . t)
   (lsp-ui-peek-enable . t)
   (lsp-ui-peek-peek-height . 20)
   (lsp-ui-peek-list-width . 50)
   (lsp-ui-peek-fontify . 'on-demand)) ;; never, on-demand, or always

  :hook ((lsp-mode-hook . lsp-ui-mode)))

(leaf lua-mode
  :straight t
  :hook (lua-mode-hook . lsp-deferred))

(leaf rustic
  :straight t
  :custom
  (rustic-default-clippy-arguments . "--benches --tests --all-targets --all-features")
  (lsp-rust-analyzer-cargo-watch-command . "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints . t)
  )

(leaf typescript-mode
  :straight t
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(leaf quickrun
  :straight t
  :commands
  (quickrun quickrun-shell quickrun-with-arg quickrun-compile-only)
  :custom
  (quickrun-focus-p . nil)
  :config
  (quickrun-set-default "c++" "c++/g++"))

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
