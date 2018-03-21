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

;;Install packages:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun install-default-pkg ()
  "Install some neat packages"
  (interactive)
  (let (package-list)
	(setq package-list '(w3m magit auctex ace-jump-mode irony company-irony company-jedi linum-relative yasnippet waher-theme google-translate use-package rust-mode flycheck-rust company-racer racer julia-mode julia-repl markdown-mode bison-mode ws-butler))
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
