;;; huan-on-save --- Some commands to run

;;; Commentary:

;; Some commands to run when a file is saved.

;;; Code:
(defun huan-do-on-save ()
  "Commands to run. Will be added to a hook."
  (cond
   ((eglot-managed-p) (eglot-format-buffer))
   ((bound-and-true-p lsp-mode) (lsp-format-buffer))
   (t (whitespace-cleanup))))
(provide 'huan-on-save)

;;; huan-on-save.el ends here
