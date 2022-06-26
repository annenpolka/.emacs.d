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

  ;; setup straight.el
    (defvar bootstrap-version)
    (let ((bootstrap-file
            (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
            (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
            (with-current-buffer
                    (url-retrieve-synchronously
                    "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                    'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf major-mode-hydra :ensure t)
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

;; flycheck syntax checking
(leaf flycheck
  :ensure t
  :global-minor-mode global-flycheck-mode)

;; (leaf flycheck-inline
;;   :ensure t
;;   :hook ((flycheck-mode-hook . flycheck-inline-mode)))
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
            (completion-cycle-threshold . 3)
            (tab-always-indent . 'complete)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

;; undo
(leaf undo-tree
  :ensure t
  :global-minor-mode global-undo-tree-mode
  )

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

  (leaf recentf
    :doc "save recent file history"
    :global-minor-mode t
    :config
    (setq recentf-save-file "~/.emacs.d/recentf"
		  recentf-max-saved-items 2000
		  recentf-auto-cleanup 'never


		  recentf-exclude
  		  '("recentf" "COMMIT_EDITMSG" "bookmarks" "\\.gitignore"
  			"\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" ".howm-keys" "\\.emacs.d/" "^/tmp/" "^/scp:"
  			(lambda (file) (file-in-directory-p file package-user-dir))))
  	(push (expand-file-name recentf-save-file) recentf-exclude))

;;; spellcheck
(leaf flyspell
  :blackout (flyspell-mode flyspell-prog-mode)
  :hook
  (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode)
  (conf-mode-hook . flyspell-prog-mode)
  (yaml-mode-hook . flyspell-prog-mode)
  :config
  (pcase (cond ((executable-find "aspell")    'aspell)
               ((executable-find "hunspell")  'hunspell)
               ((executable-find "enchant-2") 'enchant))
    (`aspell
     (setq ispell-program-name "aspell"
           ispell-extra-args '("--sug-mode=ultra"
                               "--run-together"))

     (unless ispell-aspell-dict-dir
       (setq ispell-aspell-dict-dir
             (ispell-get-aspell-config-value "dict-dir")))
     (unless ispell-aspell-data-dir
       (setq ispell-aspell-data-dir
             (ispell-get-aspell-config-value "data-dir"))))
    (`hunspell
     (setq ispell-program-name "hunspell"))
    (`enchant
     (setq ispell-program-name "enchant-2"))
    (_ (system-packages-ensure "aspell"))))
(leaf flyspell-correct
  :ensure t
  :bind
  ([remap ispell-word] . flyspell-correct-at-point))

; ╭──────────────────────────────────────────────────────────╮
; │                      language tools                      │
; ╰──────────────────────────────────────────────────────────╯
;;; lsp
(leaf lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
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
(leaf lsp-ui
    :doc "UI modules for lsp-mode"
    :url "https://github.com/emacs-lsp/lsp-ui"
    :ensure t
    :after lsp-mode
    :custom
    ((lsp-ui-doc-enable            . t)
     (lsp-ui-doc-deley             . 0.5)
     (lsp-ui-doc-header            . t)
     (lsp-ui-doc-include-signature . t)
     (lsp-ui-doc-position          . 'at-point)
     (lsp-ui-doc-max-width         . 150)
     (lsp-ui-doc-max-height        . 30)
     (lsp-ui-doc-use-childframe    . nil)
     (lsp-ui-doc-use-webkit        . nil)
     (lsp-ui-flycheck-enable       . t)
     (lsp-ui-peek-enable           . t)
     (lsp-ui-peek-peek-height      . 20)
     (lsp-ui-peek-list-width       . 50)
     (lsp-ui-peek-fontify          . 'on-demand) ;; never, on-demand, or always
     )
    :hook ((lsp-mode-hook . lsp-ui-mode))
    )

;; TODO: add cpp clangd setup
;; TODO: add rust-mode setup

; ╭──────────────────────────────────────────────────────────╮
; │                          themes                          │
; ╰──────────────────────────────────────────────────────────╯
;; font config
(defvar my/font-family "Iosevka Term")
(defvar my/font-size
  (let ((size-by-hostname
         '(("DESKTOP-B6V868U" . 14.0))))
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
  (modus-themes-hl-line . '(underline))
  (modus-themes-region . '(bg-only no-extend))
  (modus-themes-scale-headings . t)
  (modus-themes-prompts . '(background bold gray intense italic))
  (modus-themes-syntax . '(faint alt-syntax green-strings))
  :config
   (load-theme 'modus-vivendi))
; ╭──────────────────────────────────────────────────────────╮
; │                     helper interface                     │
; ╰──────────────────────────────────────────────────────────╯
(leaf display-line-numbers
    :bind ("<f9>" . display-line-numbers-mode)
    :hook ((prog-mode-hook text-mode-hook) . display-line-numbers-mode)
    :custom
    (display-line-numbers-width . 3))

(leaf hl-line
    :config
    (make-variable-buffer-local 'global-hl-line-mode)
    (add-hook 'dashboard-mode-hook (lambda() (setq global-hl-line-mode nil)))
    :global-minor-mode global-hl-line-mode)

(leaf which-key
  :doc "which-key in emacs"
  :ensure t
  :blackout which-key-mode
  :custom
  (which-key-idle-delay . 0.5)
  :init
  (which-key-setup-minibuffer)
  (which-key-mode t))

(leaf hl-todo
  :doc "TODO keywords highlighting"
  :ensure t
  :global-minor-mode global-hl-todo-mode)

(leaf highlight-indent-guides
  :doc "indent lines"
  :ensure t
  :blackout t
  :hook (((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode))
  :custom (
           (highlight-indent-guides-method . 'bitmap)
           (highlight-indent-guides-auto-enabled . t)
           (highlight-indent-guides-responsive . t)))

(leaf hl-block-mode
  :doc "blockman thing"
  :ensure t
  :global-minor-mode global-hl-block-mode)

(leaf rainbow-delimiters
  :ensure t
  :hook ((prog-mode-hook org-mode-hook) . rainbow-delimiters-mode))

;; Code folding
(leaf origami
  :ensure t
  :after evil
  :global-minor-mode global-origami-mode)
; ╭──────────────────────────────────────────────────────────╮
; │                      editing modal                       │
; ╰──────────────────────────────────────────────────────────╯
(leaf evil
  :doc "Extensible vi layer for Emacs."
  :require evil windmove undo-tree evil
  :ensure t
  :pre-setq
  ;; for evil-collection
  (evil-want-keybinding . nil)
  ;; undo system
  (evil-undo-system . 'undo-tree)
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
    ("C-s" . save-buffer)
    ("C-q" . 'evil-delete-buffer)
    ("C-l" . 'evil-ex-nohighlight)
    ("C-j" . 'evil-open-fold)
    ("C-k" . 'evil-close-fold)
    ("zg" . flyspell-correct-at-point)
    ("C-e" . dired)
    ("/" . consult-line)
    ("<leader>SPC" . 'consult-buffer)
    ("<leader>n" . 'my-mc-hydra/body)
    ("<leader>h" . windmove-left)
    ("<leader>j" . windmove-down)
    ("<leader>k" . windmove-up)
    ("<leader>l" . windmove-right))))

(leaf evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-setup-minibuffer . t)
  :config
    ;; (evil-collection-init '(magit dired consult)))
    (evil-collection-init))

(leaf smartparens
  :ensure t
  :require smartparens-config
  :global-minor-mode smartparens-global-mode)
(leaf evil-smartparens
  :ensure t
  :after smartparens
  :hook ((smartparens-enabled-hook . evil-smartparens-mode)))

(leaf evil-surround
  :require embrace evil-embrace
  :config
  (global-evil-surround-mode 1)
  (evil-embrace-enable-evil-surround-integration))
(leaf embrace
      :ensure t)
(leaf evil-embrace
      :require embrace
      :ensure t)

(leaf evil-mc
  :after evil
  :require (smartparens evil-smartparens)
  :ensure t
  :global-minor-mode global-evil-mc-mode
  :config
   (evil-define-command evil-mc-toggle-cursor-here ()
   "Create a cursor at point. If in visual block or line mode, then create
   cursors on each line of the selection, on the column of the cursor. Otherwise
   pauses cursors."
   :repeat nil
   :keep-visual nil
   :evil-mc t
   (interactive)
   (cond ((and (evil-mc-has-cursors-p)
               (evil-normal-state-p)
               (let* ((pos (point))
                       (cursor (cl-find-if (lambda (cursor)
                                           (eq pos (evil-mc-get-cursor-start cursor)))
                                           evil-mc-cursor-list)))
                   (when cursor
                   (evil-mc-delete-cursor cursor)
                   (setq evil-mc-cursor-list (delq cursor evil-mc-cursor-list))
                   t))))

           ((memq evil-this-type '(block line))
           (let ((col (evil-column))
               (line-at-pt (line-number-at-pos)))
           ;; Fix off-by-one error
           (when (= evil-visual-direction 1)
               (cl-decf col)
               (backward-char))
           (save-excursion
               (evil-apply-on-block
               (lambda (ibeg _)
                   (unless (or (= line-at-pt (line-number-at-pos ibeg))
                               (invisible-p ibeg))
                   (goto-char ibeg)
                   (move-to-column col)
                   (when (= (current-column) col)
                       (evil-mc-make-cursor-here))))
               evil-visual-beginning
               (if (eq evil-this-type 'line) (1- evil-visual-end) evil-visual-end)
               nil)
               (evil-exit-visual-state))))
           (t
           (evil-mc-pause-cursors)
           ;; I assume I don't want the cursors to move yet
           (evil-mc-make-cursor-here))))

    (evil-define-command evil-mc-undo-cursor ()
    "Undos last cursor, or all cursors in visual region."
    :repeat nil
    :evil-mc t
    (interactive)
    (if (evil-visual-state-p)
        (or (mapc (lambda (c)
                    (evil-mc-delete-cursor c)
                    (setq evil-mc-cursor-list (delq c evil-mc-cursor-list)))
                    (cl-remove-if-not
                    (lambda (pos)
                    (and (>= pos evil-visual-beginning)
                            (<  pos evil-visual-end)))
                    evil-mc-cursor-list
                    :key #'evil-mc-get-cursor-start))
            (message "No cursors to undo in region"))
        (evil-mc-undo-last-added-cursor)))

  ;; smartparens integration
   (dolist (sp-command
            '(sp-up-sexp sp-copy-sexp sp-down-sexp sp-join-sexp sp-kill-sexp sp-next-sexp sp-split-sexp sp-wrap-curly sp-wrap-round sp-raise-sexp sp-clone-sexp sp-wrap-square sp-splice-sexp sp-end-of-sexp sp-forward-sexp sp-backward-sexp sp-convolute-sexp sp-transpose-sexp sp-kill-whole-line sp-beginning-of-sexp sp-forward-barf-sexp sp-forward-slurp-sexp sp-backward-barf-sexp sp-backward-slurp-sexp sp-splice-sexp-killing-forward sp-splice-sexp-killing-backward))
     (add-to-list 'evil-mc-custom-known-commands
                  `(,sp-command
                    (:default . evil-mc-execute-call))))
   :hydra
    (my-mc-hydra (:color pink
                        :hint nil
                        :pre (evil-mc-pause-cursors))
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
    ("Q" #'evil-mc-undo-all-cursors "quit with remove all" :color blue)))

(leaf key-chord
  :ensure t
  :global-minor-mode t
  :custom
  (key-chord-two-keys-delay . 0.15)
  ;(key-chord-one-key-delay . 0.1)
  :config
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))
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

(leaf git-gutter
  :ensure t
  :global-minor-mode global-git-gutter-mode
  :custom
  '(git-gutter:ask-p . nil))
; ╭──────────────────────────────────────────────────────────╮
; │                        completion                        │
; ╰──────────────────────────────────────────────────────────╯
;; ----- minibuffer -----
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
         (orderless-smart-case . t))
  :hook ((corfu-mode-hook . my/orderless-for-corfu))
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  (defun my/orderless-for-corfu ()
    (setq-local orderless-style-dispatchers '(my/orderless-dispatch-flex-first)))
  :defer-config
  (orderless-define-completion-style orderless-default-style
         (orderless-matching-styles '(orderless-prefixes
                                        orderless-initialism
                                        orderless-regexp)))

  (orderless-define-completion-style orderless-fuzzy-style
         (orderless-matching-styles '(orderless-prefixes
                                        orderless-initialism
                                        orderless-regexp
                                        orderless-flex)))
  (setq completion-category-overrides
        '((command (styles orderless-fuzzy-style))
          (file (styles orderless-fuzzy-style))
          (buffer (styles orderless-fuzzy-style))
          (bookmark (styles orderless-fuzzy-style))
          (symbol (styles orderless-default-style))
          (consult-location (styles orderless-default-style)) ; category `consult-location' は `consult-line' などに使われる
          ;(consult-multi (styles orderless-fuzzy-style)) ; category `consult-multi' は `consult-buffer' などに使われる
          (multi-category (styles orderless-fuzzy-style))
          (unicode-name (styles orderless-default-style))
          (variable (styles orderless-default-style)))))

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
                                      consult--source-buffer
                                      consult--source-recent-file
                                      consult--source-bookmark
                                      consult--source-project-buffer)))
  )
;; affe fuzzy-finder
(leaf affe
  :config
  (leaf-handler-package affe affe nil)
  (with-eval-after-load 'consult
    (defun affe-orderless-regexp-compiler (input _type _ignorecase)
      (setq input (orderless-pattern-compiler input))
      (cons input
            (lambda (str)
              (orderless--highlight input str))))

    (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)))

;; ----- auto completion -----
(leaf corfu
  :after evil
  :ensure t
  :init
  (defun corfu-enable-in-minibuffer nil
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point
                             (list
                              (current-local-map)))
      (corfu-mode 1)))

  :hook
  ;; (minibuffer-setup-hook . corfu-enable-in-minibuffer)
  (after-init-hook . global-corfu-mode)
  :custom
  (corfu-cycle . t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto . t)                 ;; Enable auto completion
  (corfu-auto-delay . 0.01)                 ;; Enable auto completion
  (corfu-count . 15)                        ;; show more candidates
  (corfu-quit-at-boundary . t) ;; nil: スペースを入れてもquitしない
  ;; (corfu-quit-no-match . nil) ;; nil: マッチしないとき"no match"を表示してquitしない
  ;; (corfu-auto-prefix . 3)
  (corfu-preview-current . t)    ;; current candidate preview
  (corfu-preselect-first . t)    ;; candidate preselection
  (corfu-quit-no-match . 'separator)
  (corfu-separator . ?\s)
  :bind ((:corfu-map
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

(leaf corfu-doc
  :ensure t
  :hook (corfu-mode-hook . corfu-doc-mode))

(leaf yasnippet
  :ensure t
  :bind (:yas-keymap
         ("<tab>" . nil)
         ("TAB" . nil)
         ("<backtab>" . nil)
         ("S-TAB" . nil)
         ("C-o" . yas-next-field-or-maybe-expand))
  :global-minor-mode yas-global-mode)

(leaf cape
  :ensure t
  :init
  (defun my/basic-super-capf ()
    (add-to-list 'completion-at-point-functions (cape-super-capf
                                                ;; #'lsp-completion-at-point
                                                #'cape-file
                                                #'cape-tex
                                                #'cape-dabbrev
                                                #'cape-keyword
                                                #'cape-abbrev
                                                ;#'cape-ispell
                                                ;#'cape-dict
                                                ;#'cape-line
                                                #'cape-symbol)))
  :hook
  (prog-mode-hook . my/basic-super-capf)
  )


(leaf kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
