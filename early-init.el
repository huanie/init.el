;;; early-init.el --- Early init                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Huan Thieu Nguyen

;; Author: Huan Thieu Nguyen <huantn@fedora>
;; Keywords:

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

(setq use-dialog-box t
      use-file-dialog t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-buffer-menu t)

(tool-bar-mode -1)

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.
(defvar normal-gc-cons-threshold gc-cons-threshold)

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold normal-gc-cons-threshold)))

(setq package-enable-at-startup nil)

;;; early-init.el ends here
