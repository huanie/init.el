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
  Need this because defcustom, defvar etc
  will not be evaluated with default `eval-buffer'."
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
  (customize-set-variable 'eglot-extend-to-xref t)
  (customize-set-variable 'eglot-autoshutdown t))
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

;; Rust
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

;; Emacs Lisp
(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;;; config.el ends here
