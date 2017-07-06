;;Install packages:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun install-default-pkg ()
  "Install some neat packages"
  (interactive)
  (let (package-list)
	(setq package-list '(w3m magit auctex ace-jump-mode irony company-irony company-jedi linum-relative yasnippet waher-theme google-translate use-package))
	(package-refresh-contents)
	(package-initialize)
	(dolist (package package-list)
	  (unless (package-installed-p package)
		(package-install package)))))

;; After install ;;

;; *buntu/debian:
;; sudo apt-get install libclang-3.5-dev cmake python-virtualenv python-jedi python3-jedi w3m clang

;; debian:
;; sudo apt-get install python3-virtualenv

;; archlinux:
;; sudo pacman -S python-virtualenv python2-virtualenv python-jedi python2-jedi cmake clang w3m

;; emacs:
;; M-x irony-install-server
;; M-x jedi:install-server

;;;;
