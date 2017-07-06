;;;; init.el content ;;;;

;; KDE full screen fix ;;

;; If your emacs version is < 24.4

;; (defun my-fullscreen ()
;;   (interactive)
;;   (set-frame-parameter nil 'fullscreen 'fullboth) this makes the frame go fullscreen
;;   (tool-bar-mode -1) these 3 lines turn off GUI junk
;;   (scroll-bar-mode -1)
;;   (menu-bar-mode -1))

;; (defun my-non-fullscreen ()
;;   (interactive)
;;   (set-frame-parameter nil 'width 82)
;;   (set-frame-parameter nil 'fullscreen 'fullheight)
;;   (menu-bar-mode t)) I don't turn tool-bar and scroll-bar back on b/c I never want them

;; (defun toggle-fullscreen ()
;;   (interactive)
;;   (if (eq (frame-parameter nil 'fullscreen) 'fullboth)  tests if already fullscreened
;;       (my-non-fullscreen)
;;     (my-fullscreen)))

;; (global-set-key (kbd "<f11>") 'toggle-fullscreen)

;;;;

;; Package Initialization ;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;;;

;; Indentation Style ;;

(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(setq-default tab-always-indent t)
(setq c-default-style "linux" c-basic-offset 4)

;;(setq vhdl-indent-tabs-mode t)
;;(defvaralias 'c-basic-offset 'tab-width)

;;;;

;; Window Style ;;

;; Modified waher theme:

;;`(default                             ((((class color) (min-colors 88)) (:background "#000000" :foreground "#CEDBE7"))
;;`(default                             ((((class color) (min-colors 88)) (:background "#000000" :foreground "green"))

;; sha256sum original: 7b4d9b8a6ada8e24ac9eecd057093b0572d7008dbd912328231d0cada776065a
;; sha256sum modified: 0620c07c70373d3ce4ddca072de8bdfcac4438810d2acf46db8c72b0a125140d

(when (display-graphic-p) ;;Only on gui
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(custom-enabled-themes (quote (waher)))
   '(custom-safe-themes
	 (quote
	  ("0620c07c70373d3ce4ddca072de8bdfcac4438810d2acf46db8c72b0a125140d" default))))
  ;; Disable scrollbar
  (scroll-bar-mode -1))

;; Starts emacs maximixed
(add-to-list 'default-frame-alist '(fullscreen . maximized))
  
;; Disable tool-bar
(tool-bar-mode -1)

;; Show parenthesis match
(show-paren-mode 1)

;; Disable menu bar
(menu-bar-mode -1)

;; Disable the sound bell
(setq visible-bell t)

;; Display time
(display-time-mode 1)

;; Time format
(setq display-time-format "%H:%M")

;; Overwrite selected text
(delete-selection-mode t)

;; Changes the color for the minibuffer
(set-face-foreground 'minibuffer-prompt "magenta")

;; Decode GDB octal escapes
(setq gdb-mi-decode-strings t)

;;;;

;; Shortcuts ;;

;; Comment region
(global-set-key (kbd "M-[") 'comment-region)

;; Uncomment region
(global-set-key (kbd "M-]") 'uncomment-region)

;; Toggles the relative line numbering
(global-set-key (kbd "C-c l") 'linum-mode)

;; Replace string keystroke
(global-set-key (kbd "C-c s") 'replace-string)

;; Query replace string keystroke 
(global-set-key (kbd "C-c q") 'query-replace)

;; Adjust window size keystrokes
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

;; Simple buffer navigation keystrokes
(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "M-,") 'previous-buffer)

;; Change window keystroke
(global-set-key (kbd "M-ç") 'other-window)

;;;;

;; Packages config ;;

;; Enables ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c v") 'ace-jump-mode)
(global-set-key (kbd "<menu>") 'ace-jump-mode)

;; Enables images with w3m
(setq w3m-default-display-inline-images t)

;; Relative line numbering
(require 'linum-relative)

;; Enables yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Enables company and company-irony
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'after-init-hook 'global-company-mode)

;; Sets a delay of 0.3 seconds for company
(setq company-idle-delay 0.3)

;; Enable the spelling checker on commit messages
(add-hook 'git-commit-mode-hook #'(lambda ()
									 (ispell-change-dictionary "en_US")
									 (flyspell-mode)))

;; Enables company-jedi
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; Enables flyspell on latex files
(require 'tex)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Enables ido
(require 'ido)
(ido-mode t)

;; Enables ido flexible matching
(setq ido-enable-flex-matching t)

;; Ignore some buffers
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
						   "*Messages*" "Async Shell Command"))

;; Ido buffer navigation keystroke
(global-set-key (kbd "C-ç") 'ido-switch-buffer)

;; Irony configuration
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Enables google-translate
(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key (kbd "C-c t") 'google-translate-at-point)
(global-set-key (kbd "C-c T") 'google-translate-query-translate)

;; Relative line numbering mode
(require 'linum-relative)
(linum-relative-on)

;; Magit status buffer
(global-set-key (kbd "C-x g") 'magit-status)

;;;;

;; Insert Funtions ;;

;; Duplicate line function
(defun duplicate-line()
  (interactive)
  (let (sline)
	(setq sline (buffer-substring (point-at-bol) (point-at-eol)))
	(end-of-line)
	(newline)
	(insert sline)))
(global-set-key (kbd "C-c d") 'duplicate-line)

;; Curly brackets insertion
(defun insert-curly-brackets()
  (interactive)
  (end-of-line)
  (newline)
  (insert "{")
  (indent-according-to-mode)
  (newline)
  (indent-according-to-mode)
  (newline)
  (insert "}")
  (indent-according-to-mode)
  (previous-line)
  (end-of-line))
(global-set-key (kbd "C-c c") 'insert-curly-brackets)

;; Insert pairs
(defun my-insert-pair (open close)
  (if (region-active-p)
	  (progn
		(let* ((mark-start (region-beginning))
			   (mark-end (region-end)))
		  (goto-char mark-end)
		  (insert close)
		  (goto-char mark-start)
		  (insert open)))
	(insert open close))
  (backward-char))

;; Parenthesis insertion
(defun paren-insert()
  (interactive)
  (my-insert-pair ?( ?)))
(global-set-key (kbd "C-c p") 'paren-insert)

;; Double quotes insertion
(defun insert-quotes()
  (interactive)
  (my-insert-pair  ?" ?"))
(global-set-key (kbd "C-c a") 'insert-quotes)

;; Braket insertion
(defun insert-brackets()
  (interactive)
  (my-insert-pair ?[ ?]))
(global-set-key (kbd "C-c b") 'insert-brackets)


;; C/C++ Comments
(defun my-comments()
  (interactive)
  (insert "/*")
  (newline)
  (insert "* ")
  (indent-according-to-mode)
  (newline)
  (insert "*/")
  (indent-according-to-mode)
  (previous-line)
  (end-of-line))
(global-set-key (kbd "C-c i") 'my-comments)

;;;;

;; Other functions ;;

;; Automatic header creation

;; Automatic header creation
(defun create-c-header (headername)
  "Create a simple C header template"
  (interactive "sHeader name: ")
  (let ((finalname))
	(if (string= (substring headername -2 nil) ".h")
		(setq finalname (substring headername 0 -2))
	  (setq finalname headername))
	(if (string= (substring headername -4 nil) ".hpp")
		(setq finalname (substring headername 0 -4))
	  (setq finalname headername))
	(create-file-buffer (concat finalname ".h"))
	(switch-to-buffer (concat finalname ".h"))
	(insert "/*\n * Description\n */\n")
	(insert "\n#ifndef " (upcase finalname) "_H_\n")
	(insert "#define " (upcase finalname) "_H_\n")
	(insert "\n#endif")))

(defun create-cpp-header (headername)
  "Create a simple C++ header template"
  (interactive "sHeader name: ")
  (let ((finalname))
	(if (string= (substring headername -2 nil) ".h")
		(setq finalname (substring headername 0 -2))
	  (setq finalname headername))
	(if (string= (substring headername -4 nil) ".hpp")
		(setq finalname (substring headername 0 -4))
	  (setq finalname headername))
	(create-file-buffer (concat finalname ".hpp"))
	(switch-to-buffer (concat finalname ".hpp"))
	(insert "/*\n * Description\n */\n")
	(insert "\n#ifndef " (upcase finalname) "_HPP_\n")
	(insert "#define " (upcase finalname) "_HPP_\n")
	(insert "\n#endif")))

;; Move the current line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(global-set-key (kbd "M-p") 'move-line-up)

;; Move the current line down
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
(global-set-key (kbd "M-n") 'move-line-down)

(defun csv-extract-hearder (&optional separator)
  (if (null separator)
	  (setq separator ";"))
  (split-string (buffer-substring (point) (point-at-eol)) separator))

;; Converts a CSV table into LaTeX
(defun csv-to-latex (&optional separator)
  "Converts a CSV table into LaTeX"
  (interactive "sSeparator: ")
  (if (null separator)
	  (setq separator ";"))
  (let (header dim cols rows ref desc)
	(while (search-forward-regexp (concat "[0-9]+x[0-9]+" separator) (point-max) t)
	  (beginning-of-line)
	  (setq header (csv-extract-hearder separator))
	  (setq dim (pop header))
	  (setq cols (string-to-int (nth 1 (split-string dim "x"))))
	  (setq rows (string-to-int (nth 0 (split-string dim "x"))))
	  (setq ref (pop header))
	  (setq desc (pop header))
	  (kill-line)
	  (insert "\\begin{center}\n  \\begin{tabular}{|")
	  (let ((prow 0) (pcol 0) col-cells)
		(while (< pcol cols)
		  (insert "c|")
		  (setq pcol (+ pcol 1)))
		(setq pcol 0)
		(insert "}\n    \\hline")
		(next-line)
		(beginning-of-line)
		(while (< prow rows)
		  (insert "    ")
		  (setq col-cells (split-string (buffer-substring (point) (point-at-eol)) separator))
		  (kill-line)
		  (while (< (+ 1 pcol) cols)
			(insert (pop col-cells))
			(insert " & ")
			(setq pcol (+ pcol 1)))
		  (insert (pop col-cells))
		  (insert " \\\\ \\hline")
		  (setq pcol 0)
		  (setq prow (+ prow 1))
		  (next-line)
		  (beginning-of-line)))
	  (insert "  \\end{tabular}\n  \\captionof{table}{")
	  (insert desc)
	  (insert "}\n  \\label{")
	  (insert ref)
	  (insert "}\n\\end{center}\n"))))

;;;;

;; Layout ;;

(defun perfect-window-size ()
  "Finds the perfect window size"
  (let (winsize)
	(setq winsize (- (frame-width) 82))
	(if (>= winsize 100)
		winsize
	    (round (* 11 (/ (frame-width) 20.0))))))

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

;; Edit the set-window-width call to the correct width
(defun default-layout ()
  (interactive)
  (split-window-horizontally)
  (set-window-width (perfect-window-size))
  (other-window (- 1))
  (split-window-vertically)
  (shell)
  (other-window 1)
  (switch-to-buffer "*scratch*")
  (other-window (- 1)))
(global-set-key (kbd "C-c w") 'default-layout)

;;;;

;; Misc ;;

;; Windmove - Shift + arrow to change window
(windmove-default-keybindings)

;;;;
