;;; huan-kill-all-buffers --- Kill all buffers except the current focused one.

;; Commentary:

;;; Code:
(defun huan-kill-all-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(provide 'huan-kill-all-buffers)
