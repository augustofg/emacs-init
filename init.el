;; Copyright © 2017 Augusto Fraga Giachero
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

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

(when (display-graphic-p) ;;Only on gui
  ;; Starts emacs maximixed
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; Load the waher-theme
  (require 'waher-theme)
  (load-theme 'waher t)
  ;; Disable the scroll bar
  (scroll-bar-mode -1))
  
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

;; Tell custom to don't write its stuff into init.el
(setq custom-file "~/.emacs.d/custom-init.el")

;; Disable the startup screen
(setq inhibit-startup-screen t)

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

;; Compile Command
(global-set-key "\C-x\C-m" 'compile)

;;;;

;; Packages config ;;

;; Enable deleting trailing whitespace on all programming modes
(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this)

;; Load mu4e
(require 'mu4e)

;; Maildir location
(setq mu4e-maildir (expand-file-name "~/mbsync"))

;; Configure Gnus dired to allow attaching files with mu4e
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;;command used to get mail
;; use this for testing
(setq mu4e-get-mail-command "true")
;; use this to sync with mbsync
(setq mu4e-get-mail-command "mbsync augusto-gmail")

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-trash-folder  "/[Gmail].Lixeira")
(setq mu4e-headers-results-limit 500)
(setq mu4e-use-fancy-chars nil)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/queue to set up first
(setq smtpmail-queue-mail nil  ;; start in normal mode
      smtpmail-queue-dir   "~/mbsync/augusto-gmail")


;; Sort thread by date
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))

;; Load gnus configuration if available
(let (gnus-personal)
  (progn
	(setq gnus-personal (concat default-directory ".emacs.d/gnus-personal.el"))
	(if (file-exists-p gnus-personal)
		 (load gnus-personal))))

;; Defaults to octave-mode when opening .m files
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

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

;; Sets a delay of 0.2 seconds for company
(setq company-idle-delay 0.2)

;; Enable the spell checker on commit messages
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
(add-hook 'LaTeX-mode-hook '(lambda ()
							  (interactive)
							  (local-set-key (kbd "C-c m") '(lambda ()
															  (interactive)
															  (my-insert-pair ?$ ?$)))))

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

;; Rust autocomplete
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(setq racer-rust-src-path "/usr/src/rust/src/")

;; Irony configuration
(setq company-irony-ignore-case t)
(add-hook 'c++-mode-hook 'my-irony-mode)
(add-hook 'c-mode-hook 'my-irony-mode)
(add-hook 'objc-mode-hook 'my-irony-mode)

(defun my-irony-mode ()
  (when (member major-mode irony-supported-major-modes)
    (irony-mode 1)))

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

;; Insert Functions ;;

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
(defun insert-curly-braces()
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
(global-set-key (kbd "C-c c") 'insert-curly-braces)

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

(defun insert-curly-braces-next ()
  "Insert a pair of curly-braces next to each other"
  (interactive)
  (my-insert-pair ?{ ?}))
(global-set-key (kbd "C-c n") 'insert-curly-braces-next)

;; Parenthesis insertion
(defun paren-insert()
  (interactive)
  (my-insert-pair ?( ?)))
(global-set-key (kbd "C-c p") 'paren-insert)

;; Double quotes insertion
(defun insert-double-quotes()
  (interactive)
  (my-insert-pair  ?" ?"))
(global-set-key (kbd "C-c a") 'insert-double-quotes)

;; Simple quotes insertion
(defun insert-simple-quotes()
  (interactive)
  (my-insert-pair  ?' ?'))
(global-set-key (kbd "C-c x") 'insert-simple-quotes)

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

(defun spell (language)
  "Enable flyspell in this buffer for english or portuguese"
  (interactive "sLanguage [eng]: ")
  (if (string= "pt" language)
	  (ispell-change-dictionary "pt_BR"))
  (if (or (string= "en" language) (string= "" language))
	  (ispell-change-dictionary "en_US"))
  (flyspell-mode 1))

;; Automatic header creation
(defun create-c-header (headername)
  "Create a simple C header template"
  (interactive "sHeader name: ")
  (let ((finalname))
	(if (string= (substring headername -2 nil) ".h")
		(setq finalname (substring headername 0 -2))
	  (progn
		(if (string= (substring headername -4 nil) ".hpp")
			(setq finalname (substring headername 0 -4))
		  (setq finalname headername))))
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
	  (progn
		(if (string= (substring headername -4 nil) ".hpp")
			(setq finalname (substring headername 0 -4))
		  (setq finalname headername))))
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
  (other-window -1)
  (split-window-vertically)
  (shell)
  (other-window -1)
  (switch-to-buffer "*scratch*")
  (other-window -1))
(global-set-key (kbd "C-c w") 'default-layout)

;;;;

;; Misc ;;

;; Windmove - Shift + arrow to change window
(windmove-default-keybindings)

;;;;
