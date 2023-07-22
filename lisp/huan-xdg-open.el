;;; xdg-open --- Open file or directory

;;; Commentary:

;; Open a file in an external program.  Open a directory in an external file manager.
;;; Code:
(defun huan-open (file-or-dir)
  "Open current FILE-OR-DIR in external app."
  (let ((path file-or-dir))
    (start-process "" nil
		   "/usr/bin/xdg-open"
		   (if path path (expand-file-name default-directory)))))

(defun huan-open-directory ()
  "Open directory in external app."
  (interactive)
  (huan-open default-directory))

(defun huan-open-dwim ()
  "Open current file or directory in external app."
  (huan-open (buffer-file-name)))

(provide 'huan-xdg-open)

;;; huan-xdg-open.el ends here
