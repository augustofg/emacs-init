;; Copyright © 2017-2022 Augusto Fraga Giachero
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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun install-default-pkg ()
  "Install some neat packages"
  (interactive)
  (let (package-list)
	(setq package-list '(w3m magit auctex company-jedi linum-relative yasnippet waher-theme use-package rust-mode flycheck-rust julia-mode julia-repl markdown-mode bison-mode ws-butler multiple-cursors))
	(package-refresh-contents)
	(package-initialize)
	(dolist (package package-list)
	  (unless (package-installed-p package)
		(package-install package)))))

;; After install ;;

;; *buntu/debian:
;; sudo apt-get install cmake python-virtualenv python3-jedi w3m clangd
;; pip3 install python-lsp-server

;; debian:
;; sudo apt-get install python3-virtualenv

;; archlinux:
;; sudo pacman -S python-virtualenv python-jedi cmake clang w3m

;; emacs:
;; M-x jedi:install-server

;;;;
