;;; package --- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;;;

;;; Code:

;; custom code
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(defun huan/visit-config ()
  "Visit this file."
  (interactive)
  (find-file (locate-user-emacs-file "config.el")))

(defun huan/eval-buffer ()
  "Execute the current buffer as Lisp code.
Need this because defcustom,
defvar etc will not be evaluated with default `eval-buffer'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-last-sexp nil))))

;; chinese
(use-package pyim)
(use-package pyim-basedict
  :config
  (pyim-basedict-enable))

;; editing
(set-default-coding-systems 'utf-8)
(save-place-mode 1)
(electric-pair-mode 1)
(global-visual-wrap-prefix-mode 1)
(global-visual-line-mode 1)
(global-subword-mode 1)
(delete-selection-mode 1)
(column-number-mode 1)
(add-hook 'before-save-hook #'whitespace-cleanup)
(setopt tab-always-indent 'complete)
(use-package edit-indirect)
(use-package surround
  :bind-keymap
  ("C-'" . surround-keymap))
(use-package vundo)
(setopt undo-limit 100000000)
(use-package expreg
  :bind
  (("C-=" . expreg-expand)
   ("C-+" . expreg-contract)))
(use-package editorconfig
  :config
  (editorconfig-mode 1))
(pixel-scroll-precision-mode 1)
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)
(use-package separedit
  :bind (:map prog-mode-map
	      ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'markdown-mode))

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;; unclutter
(setopt native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
(tool-bar-mode -1)

;; frame, windows, buffers
(when (eq system-type 'darwin)
  (setq frame-resize-pixelwise t))
(tab-bar-mode 1)
(winner-mode 1)

;; theme
(load-theme 'modus-operandi)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon nil))

;; user interface enhancements
(setopt x-stretch-cursor t)
(setopt visible-bell t)
(setopt isearch-lazy-count t)
(setopt show-paren-style 'parenthesis)

;; minibuffer completion
(setopt completion-styles '(basic substring initials flex))
(setopt read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t)
(setopt completions-detailed t)
(setopt completion-category-overrides
	'((file (styles . (basic partial-completion)))
	  (bookmark (styles . (basic substring)))
	  (library (styles . (basic substring)))
	  (imenu (styles . (basic substring)))
	  (kill-ring (styles . (emacs22)))
	  (eglot (styles . (emacs22 substring)))))
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-find)                  ;; Alternative: consult-fd
	 ("M-s c" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )
(use-package marginalia
  :init
  (marginalia-mode))
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode 1))
(use-package vertico-mouse
  :ensure nil
  :ensure nil
  :after vertico
  :init
  (vertico-mouse-mode))
(use-package vertico-directory
  :ensure nil
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
(setopt read-extended-command-predicate #'command-completion-default-include-p)
(setopt minibuffer-prompt-properties
 '(read-only t cursor-intangible t face minibuffer-prompt))
(setopt savehist-file (locate-user-emacs-file "savehist"))
(setopt history-length 500)
(setopt history-delete-duplicates t)
(setopt savehist-save-minibuffer-history t)
(savehist-mode 1)

;; files
(recentf-mode 1)
(setopt backup-directory-alist `(("." . ,(locate-user-emacs-file "backups")))
      delete-old-versions t
      version-control t)
(setopt backup-by-copying t)
(setopt delete-by-moving-to-trash t)
(setopt create-lockfiles nil)
(setopt auto-save-default t)

(setopt dired-listing-switches "-aBhl --group-directories-first")
(setopt dired-kill-when-opening-new-dired-buffer t)
(setopt dired-dwim-target t)
(global-auto-revert-mode 1)

;; programming
(use-package eglot
  :ensure nil
  :hook
  (eglot-managed-mode . eglot-inlay-hints-mode)
  :config
  (add-to-list 'eglot-server-programs
	       '((rust-ts-mode rust-mode)
		 .
		 ("rust-analyzer" :initializationOptions
		  ( :files (:excludeDirs [".flatpak-builder" "build" "_build" "builddir" "flatpak_app" "flatpak-app" ".fenv" "generated"])
		    :check (:command "clippy")
		    :imports (:granularity (:group "module"))
		    :typing (:autoClosingAngleBrackets (:enable nil)))))
	       '((c++-mode c++-ts-mode c-mode c-ts-mode)
		 .
		 ("clangd" "--experimental-modules-support" "--clang-tidy" "--compile-commands-dir=build")))
  (add-to-list 'eglot-ignored-server-capabilities :documentHighlightProvider)
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider)
  :custom
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t))
(use-package compile
  :defer t
  :ensure nil
  :hook (compilation-filter . colorize-compilation-buffer)
  :init
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  :custom
  (compilation-scroll-output t))
(use-package apheleia
  :hook
  (css-ts-mode . apheleia-mode)
  (css-mode . apheleia-mode)
  (js-json-mode . apheleia-mode)
  (rust-ts-mode . apheleia-mode)
  (rust-mode . apheleia-mode)
  (c++-mode . apheleia-mode)
  (c++-ts-mode . apheleia-mode))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'flymake-mode)
(use-package company
  :custom
  (company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend
     company-preview-if-just-one-frontend
     company-echo-metadata-frontend))
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  :config
  (global-company-mode))
(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-check-other-buffers nil)
  (dabbrev-case-fold-search nil) ;; to make dabbrev completion match case
  :config
  (add-to-list 'dabbrev-ignored-buffer-modes 'text-mode))
(global-completion-preview-mode 1)

;; treesitter
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
	(c . ("https://github.com/tree-sitter/tree-sitter-c"))
	(cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	(css . ("https://github.com/tree-sitter/tree-sitter-css"))
	(go . ("https://github.com/tree-sitter/tree-sitter-go"))
	(yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
	(html . ("https://github.com/tree-sitter/tree-sitter-html"))
	(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	(json . ("https://github.com/tree-sitter/tree-sitter-json"))
	(lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
	(make . ("https://github.com/alemuller/tree-sitter-make"))
	(ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "ocaml/src" "ocaml"))
	(python . ("https://github.com/tree-sitter/tree-sitter-python"))
	(php . ("https://github.com/tree-sitter/tree-sitter-php"))
	(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
	(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
	(ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
	(rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
	(sql . ("https://github.com/m-novikov/tree-sitter-sql"))
	(toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
	(zig . ("https://github.com/GrayJack/tree-sitter-zig"))
	(blueprint . ("https://github.com/huanie/tree-sitter-blueprint"))
	(scala . ("https://github.com/tree-sitter/tree-sitter-scala"))
	(typst . ("https://github.com/uben0/tree-sitter-typst"))
	(elixir . ("https://github.com/elixir-lang/tree-sitter-elixir"))
	(heex . ("https://github.com/phoenixframework/tree-sitter-heex"))
	(java . ("https://github.com/tree-sitter/tree-sitter-java"))
	(qmljs . ("https://github.com/yuja/tree-sitter-qmljs"))
	(swift . ("~/.config/emacs/tree-sitter/tree-sitter-swift"))))
(setopt treesit-font-lock-level 4)

;; git
(use-package transient
  :ensure (:wait t) :demand t)
(use-package magit)

;; pdf
(use-package pdf-tools
  :config
  (pdf-tools-install-noverify))

;; typst
(add-to-list 'load-path (locate-user-emacs-file "lisp/typst-ts-mode"))
(require 'typst-ts-mode)

;; terminal
(use-package vterm)
(elpaca-process-queues)
(use-package multi-vterm)
(use-package eat
  :config
  (when (eq system-type 'darwin)
    (define-key eat-semi-char-mode-map (kbd "C-h")  #'eat-self-input)
    (define-key eat-semi-char-mode-map (kbd "<backspace>") (kbd "C-h"))))

(use-package aweshell
  :ensure (:host github :repo "manateelazycat/aweshell"))

;; Rust
(use-package rust-mode
  :custom
  (rust-mode-treesitter-derive t)
  :config
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (flymake-mode -1)
	      (indent-tabs-mode -1)))
  (add-hook 'rust-ts-mode-hook
	    (lambda ()
	      (flymake-mode -1))))

;; Emacs Lisp
(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;; Python
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; C++
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-ts-mode))

;; plantuml
(use-package plantuml-mode
  :mode "\\.puml\\'"
  :config
  (plantuml-set-output-type "png"))

;; markdown
(use-package markdown-mode)

;;; config.el ends here
