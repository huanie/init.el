;;; huan-hungry-delete --- hungry delete

;;; Commentary:

;; hungry delete that I never use.

;;; Code:
(defun huan-hungry-delete-forward ()
  "Kill all following white-space."
  (interactive)
  (huan-hungry-delete-p t))

(defun huan-hungry-delete-backward ()
  "Kill all preceding white-space."
  (interactive)
  (huan-hungry-delete-p nil))

(defun huan-hungry-delete-p (forward-p)
  "Kill all following whitespace if FORWARD-P t.
If FORWARD-P nil kill all preceding white-space."
  (let (begin end)
    (setq begin (point))
    (if forward-p
	(progn
	  (forward-word)
	  (backward-word))
      (progn
	(backward-word)
	(forward-word)))
    (setq end (point))
    (kill-region begin end)))

(provide 'huan-hungry-delete)
;;; huan-hungry-delete.el ends here
