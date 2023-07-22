;;; huan-doxygen.el  --- Generate doxygen comments with treesitter -*- lexical-binding: t -*-

;;; Commentary:
;; `huan-doxygen-function-comment' is an interactive function to generate a doxygen comment for functions.

;;; Code:

(require 'treesit)

(defun huan-doxygen-function-comment ()
  "Insert a doxygen comment block above function at point.  Do nothing if point is not on a function."
  (interactive)
  (let ((function-root (treesit-parent-until
			;; replace with param later
			(treesit-node-at (point) 'cpp)
			(lambda (node)
			  (string-match "\\(declaration\\|field_declaration\\)" (treesit-node-type node))))))
    (when function-root
      (let*
	  ((nodes (treesit-query-capture
		   function-root
		   "
(_ type: (_) @type
(function_declarator
(parameter_list) @param-list
(_)? @trail-type))
"))
		  (return? (huan-doxygen-return? nodes))
		  (params (if nodes (huan-doxygen-parameter-list
				     (alist-get 'param-list nodes))
			    nil))
		  (comment-string (if nodes (format "/* @brief%s\n%s */"
					  (if params (concat "\n" params) "")
					  (if return? " * @return\n" ""))
				    nil))
		  (insert-pos (treesit-node-start function-root)))
	(when comment-string
	  (save-excursion
            (goto-char insert-pos)
	    (insert comment-string)
	    (newline-and-indent)
	    (indent-region insert-pos (point))))))))

(defun huan-doxygen-return? (node-alist)
  "Return t if the first type specifier or the trail type in NODE-ALIST is void."
  (let ((return-type (treesit-node-text (alist-get 'type node-alist) t))
	(trailing-type (treesit-node-text (alist-get 'trail-type node-alist) t)))
    (not
     (or (string-equal return-type "void")
	 (string-equal trailing-type "void")
	 (and
	  (string-equal return-type "auto")
	  (not trailing-type))))))

(defun huan-doxygen-parameter-list (parameter-list)
  "Parse PARAMETER-LIST and return all the identifier names in one string in form of ` * @param IDENT0\\n * @param IDENT1...'.  When there are no identifiers the function will return nil."
  (let ((identifier-nodes (treesit-query-capture parameter-list
						 "
(parameter_list
(parameter_declaration declarator: (_) @ident))
")))
    (if identifier-nodes
        (mapconcat (lambda (pairs)
		  (concat " * @param "  (string-replace "&" "" (treesit-node-text (cdr pairs) t))))
				   identifier-nodes "\n")
      nil)))

(provide 'huan-doxygen)
;;; huan-doxygen.el ends here
