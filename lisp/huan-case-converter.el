;;; huan-case-converter.el --- Automatically convert case to another case  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Huan Thieu Nguyen

;; Author: Huan Thieu Nguyen <huantn@fedora>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'rx)

(defun huan--case-convert (kebab-fun snake-fun camel-fun pascal-fun &optional str)
  "Transform `STR', region or word at point in that precedence when available.

Functions accept the `STR'.
`KEBAB-FUN' should transform a kebeb-case `STR'.
`SNAKE-FUN' should transform a snake_case `STR'.
`CAMEL-FUN' should transform a camelCase `STR'.
`PASCAL-FUN' should transform a PascalCase `STR'."
  (let* ((word (cond
	       (str str)
	       ((use-region-p)
		(buffer-substring-no-properties (region-beginning) (region-end)))
	       (t (thing-at-point 'symbol t))))
	(bounds (bounds-of-thing-at-point 'symbol))
	(case-type (huan--case-identify word)))
    (when word
      (setq word (funcall
		  (pcase case-type
		    ('camel-case camel-fun)
		    ('pascal-case pascal-fun)
		    ('kebab-case kebab-fun)
		    ('snake-case snake-fun))
		  word))
      (if (and word (not str))
	  (progn
	    (if (use-region-p)
		(save-excursion
		  (kill-region (region-beginning) (region-end))
		  (insert word))
	      (save-excursion
		(kill-region (car bounds) (cdr bounds))
		(insert word))))
	word))))

(defun huan--case-already-present (case)
  "Helper message that the word does not need to be converted into `CASE'."
  (lambda (word)
    (message "%s is already %s" word case)
    nil))

(defun huan--case-remove-separator (separator case)
  "Helper function for pascal case and camel case.
Gives a function that capitalizes the argument's letters after each `SEPARATOR'.
Using `CASE' to differentiate between camel case and pascal case."
  (lambda (word)
    (let ((result (replace-regexp-in-string
		  (rx-to-string
		   `(group (+ ,separator)
			   (not ,separator)))
		  (lambda (match)
		    (capitalize (string-replace separator "" match)))
		  word)))
      (if (eq 'pascal-case case)
	  (format "%c%s"
		  (capitalize (seq-elt result 0))
		  (substring-no-properties result 1 (length result)))
	result))))

(defun huan-case-to-camel-case (&optional str)
  "Convert word at point, region or optional `STR' argument to camelCase.
This function will try its best to identify which case it converts from."
  (interactive)
    (huan--case-convert
     (huan--case-remove-separator "-" 'camel-case)
     (huan--case-remove-separator "_" 'camel-case)
     (huan--case-already-present "camel case")
     (lambda (word) (format "%c%s"
			    (downcase (seq-elt word 0))
			    (substring-no-properties word 1 (length word))))
     str))

(defun huan-case-to-pascal-case (&optional str)
  "Convert word at point, region or optional `STR' argument to PascalCase.
This function will try its best to identify which case it converts from."
  (interactive)
  (huan--case-convert
   (huan--case-remove-separator "-" 'pascal-case)
   (huan--case-remove-separator "_" 'pascal-case)
   (lambda (word)
     (format "%c%s"
	     (capitalize (seq-elt word 0))
	     (substring-no-properties word 1 (length word))))
   (huan--case-already-present "pascal case")
   str))

(defun huan--case-add-separator-remove-caps (separator)
  "Helper function for snake and kebab case.
Gives a function that separates each new word with `SEPARATOR'
 and downcases everything."
  (lambda (word)
    (let ((case-fold-search nil))
      (downcase (replace-regexp-in-string
		 (rx (group lower) (group upper))
		 (lambda (_)
		   (concat "\\1" separator "\\2"))
		 word)))))

(defun huan-case-to-snake-case (&optional str)
  "Convert word at point, region or optional `STR' argument to snake_case.
This function will try its best to identify which case it converts from."
  (interactive)
  (huan--case-convert
   (lambda (word)
     (replace-regexp-in-string "-" "_" word))
   (huan--case-already-present "snake case")
   (huan--case-add-separator-remove-caps "_")
   (huan--case-add-separator-remove-caps "_")
   str))

(defun huan-case-to-kebab-case (&optional str)
  "Convert word at point, region or optional `STR' argument to kebab-case.
This function will try its best to identify which case it converts from."
  (interactive)
  (huan--case-convert
   (lambda (word)
     (replace-regexp-in-string "_" "-" word))
   (huan--case-already-present "kebab case")
   (huan--case-add-separator-remove-caps "-")
   (huan--case-add-separator-remove-caps "-")
   str))

(defun huan--case-identify (str)
  "Identify which case `STR' is in.
When `STR' is `example' it will default to `camelCase'.
when `STR' is `Example' it will default to `PascalCase'.

When `STR' has both `-' and `_' then the char with the most
occurrences wins, when they are equal `snake_case' will win."
  (let ((indexes  (number-sequence 0 (1- (length str))))
	(pred (lambda (equalfun)
		(lambda (index)
		  (funcall equalfun (seq-elt str index)))))
	(re (rx (or "_" "-"))))
    (if (string-match-p re str)
	(progn
	  (if (> (cl-count ?- str) (cl-count ?_ str))
	      'kebab-case
	    'snake-case))
      (progn
	(setq pred (funcall pred (lambda (char) (char-uppercase-p char))))
	(if (char-uppercase-p (seq-elt str 0))
	    'pascal-case
	  'camel-case)))))

(provide 'huan-case-converter)
;;; huan-case-converter.el ends here
