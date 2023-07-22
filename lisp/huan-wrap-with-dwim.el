;;; huan-wrap-with --- Wrap marked region with something.

;;; Commentary:

;; Has a function that can be used in other functions to wrap the marked region or the char at point with the desired strings.

;;; Code:

(defun huan-wrap-with-dwim (wrap-start wrap-end)
  "The marked region or point at char will be wrapped with WRAP-START and WRAP-END.
The strings will be inserted before and after end."
  (interactive (list
		(read-string "Start with: ")
		(read-string "End with: ")))
  (let* ((mark? (use-region-p))
	 (begin (if mark? (region-beginning) (point)))
	 (end (+ (if mark? (region-end) (point)) (length wrap-start) 1)))
    (goto-char begin)
    (insert wrap-start)
    (goto-char end)
    (insert wrap-end)))

(defun huan-org-underline-dwim ()
  "Wrap with zero with space and '_' for `org-mode' underline formatting."
  (interactive)
  (huan-wrap-with-dwim "​_" "_​"))

(defun huan-org-bold-diwm ()
  "Wrap with zero with space and '*' for `org-mode' bold formatting."
  (interactive)
  (huan-wrap-with-dwim "*​" "​*"))

(provide 'huan-wrap-with-dwim)
;;; huan-wrap-with-dwim.el ends here
