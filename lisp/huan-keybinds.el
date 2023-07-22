(defun huan-visit-config-file ()
  "Visit \"init.el\"."
  (interactive)
  (find-file (expand-file-name user-init-file)))

(evil-set-leader 'normal (kbd "SPC"))

(defun huan-evil-leaders (KEYS FUNC)
  "Create keybinds with leader key.
     KEYS is a string of keys and FUNC the symbol? to be called."
  (evil-global-set-key 'normal
		       (kbd (concat "<leader> " KEYS))
		       FUNC))

(huan-evil-leaders "x f" #'find-file)
(huan-evil-leaders "x b" #'switch-to-buffer)
(huan-evil-leaders "x 1" #'delete-other-windows)
(huan-evil-leaders "x s" #'save-buffer)

(huan-evil-leaders "x c" #'save-buffers-kill-emacs)

(huan-evil-leaders "h c" #'describe-key-briefly)
(huan-evil-leaders "h f" #'describe-function)
(huan-evil-leaders "h a" #'apropos-command)
(huan-evil-leaders "h v" #'describe-variable)

(huan-evil-leaders "<left>" #'windmove-left)
(huan-evil-leaders "<right>" #'windmove-right)
(huan-evil-leaders "<up>" #'windmove-up)
(huan-evil-leaders "<down>" #'windmove-down)

(huan-evil-leaders "x <down>" #'windmove-delete-down)
(huan-evil-leaders "x <up>" #'windmove-delete-up)
(huan-evil-leaders "x <left>" #'windmove-delete-left)
(huan-evil-leaders "x <right>" #'windmove-delete-right)

;; maybe write left and up
(huan-evil-leaders "c <right>" #'split-window-right)
(huan-evil-leaders "c <down>" #'split-window-below)

(huan-evil-leaders "x k" #'kill-current-buffer)

(huan-evil-leaders "c c" #'huan-visit-config-file)

(require 'huan-xdg-open)
(huan-evil-leaders "o e" #'huan-open-dwim)

;; where is my lexical binding...4iuy3475
(add-hook 'c-mode-hook
	  (lambda ()
	    (keymap-local-set "<f5>" (lambda ()
				       (interactive)
				       (compile "make -k")))))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (keymap-local-set "<f5>" (lambda ()
				       (interactive)
				       (compile "make -k")))))

(provide 'huan-keybinds)
