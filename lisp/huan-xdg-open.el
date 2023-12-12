;;; xdg-open --- Open file or directory

;;; Commentary:

;; Open a file in an external program.  Open a directory in an external file manager.
;;; Code:
(defun huan-open (url &optional _ignored)
  "Pass the specified URL to the \"open\" command.
open is a desktop utility that calls your preferred web browser.
The optional argument IGNORED is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (call-process "open" nil 0 nil url))

(provide 'huan-xdg-open)

;;; huan-xdg-open.el ends here
