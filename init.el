;;; package --- Summary
;;; Commentary:
;;; Stuff that I can't put in the literate config

;;; Code:
(require 'package)
(require 'org)
(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq elisp-flymake-byte-compile-load-path user-emacs-directory)

(setq use-package-always-ensure t)

(use-package gcmh
  :custom
  (read-process-output-max (* 64 1024))
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  :functions gcmh-mode
  :config
  (gcmh-mode 1))

(use-package gnu-elpa-keyring-update)

(defvar tangled-file (concat user-emacs-directory "init-tangled.el"))

(defun huan-tangle-config ()
  "Tangle the config file.
Basically inserting the code blocks into a file specified in the org document."
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load (expand-file-name custom-file)))

(define-minor-mode huan-config-mode
  "Minor mode to tangle org files on save."
  :lighter nil
  (if huan-config-mode
      (add-hook 'after-save-hook #'huan-tangle-config nil t)
    (remove-hook 'after-save-hook #'huan-tangle-config t)))

;; load the config
(if (file-exists-p tangled-file)
    (load tangled-file)
  (progn
    (org-babel-tangle-file (concat user-emacs-directory "config.org") tangled-file)
    (load tangled-file)))

;;; init.el ends here
