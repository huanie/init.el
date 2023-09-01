;;; package --- Summary
;;; Commentary:
;;; Stuff that I can't put in the literate config

;;; Code:
(require 'package)
(require 'use-package)

(package-initialize)

(setq elisp-flymake-byte-compile-load-path user-emacs-directory)

(setq use-package-always-ensure t)

(use-package gnu-elpa-keyring-update)

(defvar tangled-file (locate-user-emacs-file "init-tangled.el"))

(defun huan-tangle-config ()
  "Tangle the config file.
Basically inserting the code blocks into a file specified in the org document."
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(setq custom-file (locate-user-emacs-file "custom.el"))
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
    (org-babel-tangle-file (locate-user-emacs-file "config.org") tangled-file)
    (load tangled-file)))

;;; init.el ends here
