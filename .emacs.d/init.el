;;;  --- Summary
;;; Code:
;;; Commentary: 
(package-initialize) 

(setq package-archives '(;; ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ;; ("org" . "http://orgmode.org/elpa/")
			 ))
(setq next-line-add-newlines t)
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
(setq nrepl-hide-special-buffers nil)
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
(require 'orgconfig)

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

					; (setq font-lock-maximum-decoration t)
(setq compilation-scroll-output t)
					; (setq compile-auto-highlight t)
					; (setq auto-mode-alist (cons '(".*Makefile.*" . makefile-mode) auto-mode-alist))
					; (setq auto-mode-alist (cons '("\\.mak" . makefile-mode) auto-mode-alist))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(bind-keys 
 ("C-c n" . indent-buffer)
 ("C-x m" . magit-status)	   
 ("C-." . delete-other-windows)
 ("M-o" . other-window)
 ("C-]" . ibuffer)
 ("C-x k" . kill-this-buffer))


;; (require 'instructions)

(use-package paste-kde
  :commands paste-kde-region
  :ensure paste-kde)

(setq TeX-engine 'pdflatex)

;; (setq uniquify-buffer-name-style 'reverse)
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

(global-set-key (kbd "M-<f1>") '(lambda () (interactive)
				  (save-excursion (if (and (boundp 'recentf-mode) recentf-mode)
						      (progn (recentf-mode 0)
							     (org-publish "blog")
							     (recentf-mode +1))
						    (org-publish "blog")))))
(global-set-key (kbd "C-<f1>") '(lambda () (interactive)
				  (save-excursion (if (and (boundp 'recentf-mode) recentf-mode)
						      (progn (recentf-mode 0)
							     (org-publish-current-file)
							     (recentf-mode +1))
						    (org-publish-current-file)))))

(bind-keys :map org-mode-map
	   ("<f10>" . org-plot/gnuplot))

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
(global-set-key (kbd "<f5>") 'moz-controller-page-refresh)
(global-set-key (kbd "C-z") 'repeat)

(global-set-key (kbd "C-x g") 'rgrep)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

(setq default-directory "~/development/")
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; (setq gc-cons-threshold 20000000)

;; (load-theme 'moe-light t)
(load-theme 'leuven t)
(require 'ac-nrepl)

(projectile-global-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

(require 's)



(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; insert keybinding setup here
 			       (cljr-add-keybindings-with-prefix "C-c C-a")
                               ))

(add-hook 'clojure-mode-hook 'yas/minor-mode-on)

(require 'yasnippet)
(when (require 'yasnippet nil 'noerror)
  (progn
    (yas/load-directory "~/.emacs.d/snippets")))

(require 'geiser)
(require 'quack)
(setq geiser-active-implementations '(racket))

(defun push-mark-no-activate ()
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-+") 'push-mark-no-activate)

(defun jump-to-mark ()
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "M-+") 'jump-to-mark)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#5f5f5f" "#ff4b4b" "#a1db00" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#ffffff"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes (quote ("032fe1f3acb2dcca1d451b891e1275a7d51f62c355e7883a8342e5734153a072" "7a9f392481b6e2fb027ab9d8053ab36c0f23bf5cc1271206982339370d894c74" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "65ae93029a583d69a3781b26044601e85e2d32be8f525988e196ba2cb644ce6a" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "d5336b707d375fbc2c00a53a60cafa264fc7e600fd747cce2dde976609b37573" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "40c4ae07d79f89c8f4e35b11066e059d289da043e70a37b5e4a87b0a06f26d07" "6b1b3ef12a4a429f9d2eae2019115b0a7583563e17525f0a4e9696433f2f3c16" "b0ccdbe8d324c6d14240ef8dad9e547c7fc7cd11450eac9e6807dbce0430f3c0" "f0d90d902dbee341e375b3b5b58e3cb4c26a657894feb5eb7ff7535b1a8ce9d4" "fb7e7b68fe6e72c52bd356cb8a399771605432f31ed7d38215adf38c6da045f8" "75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "8b231ba3e5f61c2bb1bc3a2d84cbd16ea17ca13395653566d4dfbb11feaf8567" "93e458ab36b4d904c2e485944d0e1b4d4ad879d83bb6ca5c19a9dac7f6549ee5" "fa4e4895e851ffbb9ac68c2f26a863ba054a96e4f3be4cb1f03db9a0cf46fb69" default)))
 '(fci-rule-color "#383838")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors (--map (solarized-color-blend it "#fdf6e3" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(rainbow-identifiers-cie-l*a*b*-lightness 35)
 '(rainbow-identifiers-cie-l*a*b*-saturation 40)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#c85d17") (60 . "#be730b") (80 . "#b58900") (100 . "#a58e00") (120 . "#9d9100") (140 . "#959300") (160 . "#8d9600") (180 . "#859900") (200 . "#669b32") (220 . "#579d4c") (240 . "#489e65") (260 . "#399f7e") (280 . "#2aa198") (300 . "#2898af") (320 . "#2793ba") (340 . "#268fc6") (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list (quote (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))
