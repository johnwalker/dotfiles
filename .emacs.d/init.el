;;; package --- Summary
;;; Code:
;;; Commentary:

(require 'package)

(package-initialize)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(setq package-list '(ac-nrepl
		     ag
		     ample-theme
		     ample-zen-theme
		     auctex
		     auto-complete
		     babel
		     bm
		     browse-kill-ring
		     bubbleberry-theme
		     cider
		     clojure-mode
		     colorsarenice-theme
		     cyberpunk-theme
		     discover
		     dot-mode
		     elixir-mix
		     elixir-mode
		     erlang
		     ess
		     fancy-narrow
		     flx-ido
		     flx
		     flycheck-haskell
		     flycheck
		     gedit-mode
		     geiser
		     gist
		     gh
		     git-gutter
		     god-mode
		     goto-chg
		     graphviz-dot-mode
		     gruvbox-theme
		     guide-key-tip
		     guide-key
		     haskell-mode
		     helm-ls-git
		     helm-swoop
		     helm
		     highlight-parentheses
		     hiwin
		     hl-line+
		     htmlize
		     inkpot-theme
		     ir-black-theme
		     iregister
		     irfc
		     latex-preview-pane
		     launch
		     legalese
		     leuven-theme
		     logito
		     magit
		     git-rebase-mode
		     git-commit-mode
		     makey
		     malabar-mode
		     fringe-helper
		     markdown-mode+
		     markdown-mode
		     markup-faces
		     minimal-theme
		     moe-theme
		     molokai-theme
		     monokai-theme
		     move-text
		     occur-x
		     org-pomodoro
		     alert
		     gntp
		     owdriver
		     log4e
		     paredit
		     paste-kde
		     pcache
		     persp-mode
		     popup
		     popwin
		     pos-tip
		     powerline
		     pretty-mode
		     prodigy
		     f
		     projectile
		     pkg-info
		     epl
		     rainbow-delimiters
		     rings
		     rust-mode
		     s
		     shell-pop
		     shm
		     slime
		     smartparens
		     smartrep
		     smex
		     solarized-theme
		     dash
		     sr-speedbar
		     stekene-theme
		     subatomic-theme
		     sublime-themes
		     swoop
		     async
		     pcre2el
		     ht
		     tabbar
		     ttrss
		     undo-tree
		     use-package
		     diminish
		     bind-key
		     web
		     whole-line-or-region
		     workgroups
		     yaxception))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; activate all the packages (in particular autoloads)

(setq backup-directory-alist `(("." . "~/.saves")))

(mapc #'(lambda(x) (add-to-list 'load-path x))
      '("~/.emacs.d/personal"))

(package-initialize)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))

(require 'use-package)
(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-result-prefix "eval> ")

(use-package smartparens
  :ensure smartparens
  :init (progn
	  (require 'smartparens-config)
	  (bind-keys :map smartparens-mode-map
		     ("M-s" . sp-splice-sexp))))

(smartparens-global-mode +1)

(use-package undo-tree
  :ensure undo-tree
  :commands undo-tree-visualize
  :bind ("C-/" . undo-tree-visualize))

(use-package package
  :bind ("C-x u" . package-list-packages))

(defun setup-ui ()
  "Activates UI customizations."
  (interactive)
  (blink-cursor-mode 0)
  (column-number-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (scroll-bar-mode 0)))
    (progn 
      (scroll-bar-mode 0)))
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (set-default 'truncate-lines t)
  (electric-indent-mode 1)
  (setq echo-keystrokes 0.01)
  (setq frame-title-format '("%f - " user-real-login-name "@" system-name))
  (setq inhibit-startup-screen t)
  (scroll-bar-mode 0)
  (global-auto-revert-mode 1)
  (setq initial-scratch-message (with-temp-buffer
				  (insert-file-contents "~/.emacs.d/scratch")
				  (buffer-string)))
  (setq linum-format " %d ")
  (setq show-paren-delay 0)
  (setq truncate-partial-width-windows t)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (setq show-help-function nil)
  (which-function-mode t)
  (setq confirm-nonexistent-file-or-buffer nil))

(setup-ui)

(use-package discover
  :ensure discover
  :init (global-discover-mode t))

(use-package prodigy
  :defer t
  :ensure prodigy
  :bind ("¶" . prodigy)
  :config (progn
	    (prodigy-define-service
	      :name "personal_blog 1620"
	      :command "python"
	      :args '("-m" "http.server" "1620")
	      :cwd "~/documents/blog"
	      :tags '(blog)
	      :kill-signal 'sigkill
	      :kill-process-buffer-on-stop t)
	    (defun prodigy-toggle-service (service &optional force callback)
	      (let ((process (plist-get service :process)))
		(if (prodigy-service-started-p service)
		    (prodigy-stop-service service force callback)
		  (prodigy-start-service service callback))))
	    (defun prodigy-toggle (&optional force)
	      (interactive "P")
	      (prodigy-with-refresh
	       (-each (prodigy-relevant-services) 'prodigy-toggle-service)))
	    (define-key prodigy-mode-map (kbd "s") 'prodigy-toggle)))

(setq default-frame-alist '((font-backend . "xft")
			    (font . "Inconsolata-12")			    
			    (vertical-scroll-bars . 0)
			    (menu-bar-lines . 0)
			    (tool-bar-lines . 0)))

(setq font-lock-maximum-decoration t)
(setq compilation-scroll-output t)
(setq compile-auto-highlight t)
(setq auto-mode-alist (cons '(".*Makefile.*" . makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mak" . makefile-mode) auto-mode-alist))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(bind-keys 
 ("C-c n" . indent-buffer)
 ("C-x m" . magit-status)	   
 ("C-." . delete-other-windows)
 ("C-t" . other-window)
 ("C-]" . ibuffer)
 ("C-x k" . kill-this-buffer))

(require 'orgconfig)
(require 'instructions)

(use-package paste-kde
  :commands paste-kde-region
  :ensure paste-kde)

(setq TeX-engine 'pdflatex)

(setq uniquify-buffer-name-style 'reverse)
(global-rainbow-delimiters-mode)
(setq recentf-max-menu-items 150)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)

(setq byte-compile-warnings '(not nresolved
				  free-vars
				  callargs
				  redefine
				  obsolete
				  noruntime
				  cl-functions
				  interactive-only))

(setq inferior-lisp-program "sbcl")

(defvar tex-compile-commands
  '(("pdflatex --interaction=nonstopmode %f")))

(setq x-select-enable-clipboard t)
(require 'legalese)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x 4" "C-x 5" "C-x 6" "C-x 8" "C-x v"))
(guide-key-mode 1) 
(setq guide-key/idle-delay 0)
(whole-line-or-region-mode +1)

(bind-keys :map org-mode-map
	   ("<f10>" . org-plot/gnuplot)
	   ("C-c p" . (lambda () (interactive)
			(save-excursion (if (and (boundp 'recentf-mode) recentf-mode)
					    (progn (recentf-mode 0)
						   (org-publish "blog")
						   (recentf-mode +1))
					  (org-publish "blog"))))))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)

(use-package helm-swoop
  :commands helm-swoop
  :init 
  (bind-key "C-s" 'helm-swoop)	  
  :config (progn (setq helm-swoop-font-size-change: nil)
		 (setq helm-swoop-pre-input-function (lambda ()
						       "Pre input function. Utilize region and at point symbol"
						       ""))))

(setq slime-lisp-implementations '(("sbcl" ("sbcl" "--dynamic-space-size" "2048"))))

(global-set-key (kbd "C-x C-r") 'helm-recentf)
(recentf-mode 1)

(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "C-z") 'repeat)

(global-set-key (kbd "C-x g") 'rgrep)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

(setq default-directory "~/development/")
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

(global-git-gutter-mode +1)
(setq git-gutter:lighter " GG")
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

(global-auto-revert-mode t)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(setq gc-cons-threshold 20000000)

(load-theme 'leuven t)
(require 'ac-nrepl)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fa4e4895e851ffbb9ac68c2f26a863ba054a96e4f3be4cb1f03db9a0cf46fb69" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))
