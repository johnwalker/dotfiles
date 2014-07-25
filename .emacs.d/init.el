;;;  --- Summary
;;; Code:
;;; Commentary:

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:background nil))))
;;  '(cursor ((t (:foreground "red"))))
;;  '(linum ((t (:background "color-237" :foreground "yellow")))))


(add-to-list 'load-path "~/.emacs.d/personal")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))


(defun indent-and-cleanup-buffer ()
  (interactive)
  (indent-region (point-min) (point-max))
  (save-excursion
    (whitespace-cleanup)))


(fset 'push\ master
      [?\C-x ?m ?! ?! ?p ?u ?s ?h ?  ?o ?r ?i ?g ?i ?n ?  ?m ?a ?s ?t ?e ?r return ?q ?q])


(setq el-get-sources
      '(;; (:name ac-nrepl)
	;; (:name ag)
	;; (:name alert)
	;; (:name async)
	;; (:name aurel)
	(:name babel)
	(:name use-package)
	(:name bind-key
	       :after (progn
			(bind-keys
			 ("C-c n" . indent-and-cleanup-buffer)
			 ("C-x m" . magit-status)
			 ("C-." . delete-other-windows)
			 ("C-o" . other-frame)
			 ("M-o" . other-window)
			 ("C-x C-b" . ibuffer)
			 ("C-x k" . kill-this-buffer))))
	(:name bm)
	(:name browse-kill-ring)
	(:name bubbleberry-theme)
	(:name calfw)
	(:name cider
	       :before (progn
			 (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
			 (setq cider-show-error-buffer 'except-in-repl)))
	(:name cider-tracing)
	(:name clj-refactor)
	(:name clojure-mode)
	(:name cyberpunk-theme)
	(:name dash)
	(:name diminish)
	(:name dired-hacks-utils)
	(:name discover)
	(:name dot-mode)
	(:name dvorak-mode)
	(:name el-get)
	(:name epl)
	(:name erlang)
	;; (:name espresso-theme)
	(:name f)
	(:name fancy-narrow)
	(:name flx)
	(:name flycheck)
	(:name flycheck-haskell)
	(:name fringe-helper)
	(:name geiser)
	(:name gh)
	(:name gist)
	(:name git-commit-mode)
	(:name git-rebase-mode)
	(:name gntp)
	(:name gnu-apl-mode)
	(:name god-mode)
	(:name goto-chg)
	(:name graphviz-dot-mode)
	(:name gruvbox-theme)
	(:name guide-key)
	(:name guide-key-tip)
	(:name haskell-mode)
	(:name helm)
	(:name helm-ls-git)
	(:name helm-swoop)
	(:name hl-line+)
	(:name ht)
	(:name htmlize)
	(:name impatient-mode)
	(:name inkpot-theme)
	(:name ir-black-theme)
	(:name iregister)
	(:name irfc)
	(:name latex-preview-pane)
	(:name launch)
	(:name legalese)
	(:name leuven-theme)
	(:name log4e)
	(:name logito)
	(:name makey)
	(:name malabar-mode)
	(:name markdown-mode+)
	(:name markup-faces)
	(:name minimal-theme)
	(:name moe-theme)
	(:name molokai-theme)
	(:name monokai-theme
	       :after (progn
			(require 'monokai-theme)
			(load-theme 'monokai t)))
	(:name move-text)
	(:name moz)
	(:name moz-controller)
	(:name multiple-cursors)
	(:name mustache)
	;; (:name obsidian-theme)
	(:name occur-x)
	(:name org)
	(:name org-plus-contrib)
	(:name org-pomodoro)
	(:name owdriver)
	(:name paradox)
	(:name paredit)
	(:name paste-kde)
	(:name pcache)
	(:name pcre2el)
	(:name persp-mode)
	(:name popup)
	(:name popwin)
	(:name pretty-mode)
	(:name prodigy)
	(:name quack)
	(:name rainbow-delimiters)
	(:name rings)
	(:name rust-mode)
	(:name s)
	(:name shell-pop)
	(:name shm)
	(:name simple-httpd)
	(:name slime)
	(:name smartrep)
	(:name smex)
	(:name solarized-theme)
	(:name sr-speedbar)
	(:name stekene-theme)
	(:name subatomic-theme)
	(:name sublime-themes)
	(:name sublimity)
	(:name sunny-day-theme)
	(:name swoop)
	(:name tabbar)
	(:name tabulated-list)
	(:name tango-plus-theme)
	(:name ttrss)
	(:name typed-clojure-mode)
	(:name use-package)
	(:name uzumaki)
	(:name web)
	(:name workgroups)
	(:name yasnippet)
	(:name yaxception)
	(:name zop-to-char)
	(:name org-page
	       :after (progn (require 'org-page)
			     (require 'orgconfig)
			     (setq op/repository-directory "~/documents/johnwalker.github.io/")
			     (setq op/site-domain "http://johnwalker.github.io/")
			     (setq op/personal-disqus-shortname "johnwalker")))
	(:name projectile
	       :after (progn
			(require 'projectile)
			(add-hook 'clojure-mode-hook 'projectile-on)
			(add-hook 'eshell-mode-hook  'projectile-on)
			(add-hook 'magit-mode-hook   'projectile-on)))
	;; (:name espresso-theme
	;;        :after (progn (require 'espresso-theme)
	;;		     (load-theme 'espresso t)))
	(:name discover-my-major
	       :type git
	       :url "https://github.com/steckerhalter/discover-my-major")
	(:name prodigy
	       :after (progn
			(require 'prodigy)
			(global-set-key (kbd "¶") 'prodigy)
			(prodigy-define-service
			 :name "personal_blog 1620"
			 :command "python"
			 :args '("-m" "http.server" "1620")
			 :cwd "~/documents/johnwalker.github.io"
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

	(:name magit
	       :before (global-set-key (kbd "C-x m") 'magit-status))
	(:name flx-ido
	       :after (progn
			(require 'flx-ido)
			(flx-ido-mode 1)))
	(:name org-mode
	       :before (progn
			 (global-set-key (kbd "C-<f2>") 'org-capture)
			 (global-set-key (kbd "M-<f2>") 'org-agenda)
			 (global-set-key (kbd "M-<f1>") '(lambda () (interactive)
							   (op/do-publication t nil t t)))
			 ;; (global-set-key (kbd "M-<f1>") '(lambda () (interactive)
			 ;;				   (save-excursion (if (and (boundp 'recentf-mode) recentf-mode)
			 ;;						       (progn (recentf-mode 0)
			 ;;							      (org-publish "blog")
			 ;;							      (recentf-mode +1))
			 ;;						     (org-publish "blog")))))
			 ;; (global-set-key (kbd "C-<f1>") '(lambda () (interactive)
			 ;;				   (save-excursion (if (and (boundp 'recentf-mode) recentf-mode)
			 ;;						       (progn (recentf-mode 0)
			 ;;							      (org-publish-current-file)
			 ;;							      (recentf-mode +1))
			 ;;						     (org-publish-current-file)))))
			 ))
	(:name expand-region
	       :after (global-set-key (kbd "C-r") 'er/expand-region))

	(:name rainbow-mode :type elpa)
	(:name descbinds-anything
	       :after (progn
			(descbinds-anything-install)
			(global-set-key (kbd "C-h b") 'descbinds-anything)))
	(:name undo-tree
	       :after (progn
			(require 'undo-tree)
			(global-set-key (kbd "C-/") 'undo-tree-visualize)))
	(:name whole-line-or-region)
	(:name smartparens
	       :before (progn
			 (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
			 (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
			 (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
			 (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)

			 (require 'smartparens)

			 (sp-pair "'" nil :actions :rem)
			 (sp-pair "`" nil :actions :rem)
			 (add-hook 'lisp-mode smartparens-strict-mode)
			 (add-hook 'clojure-mode smartparens-strict-mode)
			 (add-hook 'org-mode smartparens-mode)
			 (mapc (lambda (mode)
				 (add-hook (intern (format "%s-hook" (symbol-name mode))) 'smartparens-strict-mode))
			       sp--lisp-modes)

			 (mapc (lambda (info)
				 (let ((key (kbd (car info)))
				       (function (car (cdr info))))
				   (define-key sp-keymap key function)))
			       '(("C-M-f" sp-forward-sexp)
				 ("C-M-b" sp-backward-sexp)
				 ("C-k" sp-kill-hybrid-sexp)
				 ("C-M-d" sp-down-sexp)
				 ("C-M-a" sp-backward-down-sexp)
				 ("C-S-a" sp-beginning-of-sexp)
				 ("C-S-d" sp-end-of-sexp)

				 ("C-M-e" sp-up-sexp)

				 ("C-M-u" sp-backward-up-sexp)
				 ("C-M-t" sp-transpose-sexp)

				 ("C-M-n" sp-next-sexp)
				 ("C-M-p" sp-previous-sexp)

				 ("C-M-w" sp-copy-sexp)

				 ("M-<delete>" sp-unwrap-sexp)
				 ("M-S-<backspace>" sp-backward-kill-symbol)

				 ("C-<right>" sp-forward-slurp-sexp)
				 ("C-<left>" sp-forward-barf-sexp)

				 ("C-M-<right>" sp-backward-barf-sexp)
				 ("M-D" sp-splice-sexp)
				 ("M-k" sp-splice-sexp-killing-around)
				 ("C-M-<delete>" sp-splice-sexp-killing-forward)
				 ("C-M-<backspace>" sp-splice-sexp-killing-backward)
				 ("C-S-<backspace>" sp-splice-sexp-killing-around)

				 ("C-]" sp-select-next-thing-exchange)
				 ("C-<left_bracket>" sp-select-previous-thing)
				 ("C-M-]" sp-select-next-thing)

				 ("M-F" sp-forward-symbol)
				 ("M-B" sp-backward-symbol)))
			 (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)))))

(global-set-key (kbd "C-c n") 'indent-and-cleanup-buffer)


(mapc (lambda (f)
	(let ((name (plist-get f :name)))
	  (when (not (require name nil t)) (el-get-install name))))
      el-get-sources)

(setq my-packages (mapcar 'el-get-source-name el-get-sources))

(el-get 'sync my-packages)

(require 'use-package)
(setq backup-directory-alist `(("." . "~/.saves")))

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))

(setq cider-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq nrepl-hide-special-buffers nil)
(setq cider-repl-result-prefix "---> ")

(defun setup-ui ()
  "Activates UI customizations."
  (interactive)
  (blink-cursor-mode 0)
  (fset 'yes-or-no-p 'y-or-n-p)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (scroll-bar-mode 0)))
    (scroll-bar-mode 0))
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

(setq default-frame-alist '((font-backend . "xft")
			    (font . "Fantasque Sans Mono-10")
			    (vertical-scroll-bars . 0)
			    (menu-bar-lines . 0)
			    (tool-bar-lines . 0)))



(global-rainbow-delimiters-mode)
(setq recentf-max-menu-items 300)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'subword-mode)

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

(whole-line-or-region-mode +1)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)

;; (use-package helm-swoop
;;   :commands helm-swoop
;;   :init
;;   (bind-key "C-s" 'helm-swoop)
;;   :config (progn (setq helm-swoop-font-size-change: nil)
;;		 (setq helm-swoop-pre-input-function (lambda ()
;;						       "Pre input function. Utilize region and at point symbol"
;;						       ""))))

(setq slime-lisp-implementations '(("sbcl" ("sbcl" "--dynamic-space-size" "2048"))))

(global-set-key (kbd "C-<tab>") 'list-command-history)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-c h") 'helm-mini)
(recentf-mode 1)

(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "C-z") 'zop-to-char)

(global-set-key (kbd "C-x g") 'ag)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; (require 'shm)
;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)


(ido-mode 1)
(ido-everywhere 1)

(setq ido-use-faces nil)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
			       (clj-refactor-mode 1)
			       (cljr-add-keybindings-with-prefix "C-c C-a")
			       ))

;; (add-hook 'clojure-mode-hook 'yas/minor-mode-on)

;; (require 'yasnippet)
;; (yas/load-directory "~/.emacs.d/snippets")

(defun push-mark-no-activate ()
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "C-+") 'push-mark-no-activate)
(global-set-key (kbd "M-+") 'jump-to-mark)
(global-set-key (kbd "C-c y") 'browse-kill-ring)
;; (global-set-key (kbd "C-r") 'er/expand-region)
;; (setq-default line-spacing 0)

;; (require 'cider)
;; (require 'cider-macroexpansion)
;; (add-hook 'cider-mode-hook 'cider-macroexpansion-minor-mode)
(global-set-key (kbd "C-h C-m") 'discover-my-major)

;; (require 'latex)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq mu4e-maildir "~/Maildir/john.lou.walker")

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
	 ("/[Gmail].Sent Mail"   . ?s)
	 ("/[Gmail].Trash"       . ?t)
	 ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
 user-mail-address "john.lou.walker@gmail.com"
 user-full-name  "John L. Walker"
 mu4e-compose-signature
 (concat
  "John L. Walker\n"
  "http://johnwalker.github.io\n"))

(require 'smtpmail)

;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; (require 'workgroups2)

;; (setq wg-prefix-key (kbd "s-s"))
;; (setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")

;; (global-set-key (kbd "s-s s-r")     'wg-reload-session)
;; (global-set-key (kbd "s-s s-s") 'wg-save-session)
;; (global-set-key (kbd "s-s s-w") 'wg-switch-to-workgroup)
;; (global-set-key (kbd "s-s s-p")         'wg-switch-to-previous-workgroup)
;; (workgroups-mode 1)

(global-set-key (kbd "s-SPC") 'rectangle-mark-mode)

(global-set-key (kbd "C-<up>") 'scroll-up-line)
(global-set-key (kbd "C-<down>") 'scroll-down-line)

(add-hook 'after-make-frame-functions
	  '(lambda (frame)
	     (modify-frame-parameters frame
				      '((vertical-scroll-bars . nil)
					(horizontal-scroll-bars . nil)))))


(set-frame-parameter (selected-frame) 'alpha '(95 50))
(add-to-list 'default-frame-alist '(alpha 100 50))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(compilation-message-face (quote default))
 '(cursor-color "#839496")
 '(custom-safe-themes
   (quote
    ("c7306fa678d07e5ce463852fac0a07c02f38bc419282999c667db245de795204" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "65ae93029a583d69a3781b26044601e85e2d32be8f525988e196ba2cb644ce6a" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "6449a21695482b9d06c72f021fedc962a43cf4946d099fb0e8336ba80ff5c481" "c2329c473e65e7c14cb7466a79a42c3f62dab7285676f771e91b00a9ceb33b28" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "032fe1f3acb2dcca1d451b891e1275a7d51f62c355e7883a8342e5734153a072" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "c739f435660ca9d9e77312cbb878d5d7fd31e386a7758c982fa54a49ffd47f6e" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "0795e2c85394140788d72d34969be4acb305e4a54149e7237787d9df27832fbb" "e98e6905d1bd8f71c62ee8967ecc8cf7fc8cfd760f0797f23ecd66810d3dd9c8" "75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "8b231ba3e5f61c2bb1bc3a2d84cbd16ea17ca13395653566d4dfbb11feaf8567" "aef74863c4b2b3aec8a4941d58682ec11c04e8ae1f6a9d8e5278ccef8913b377" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" default)))
 '(fci-rule-color "#383838")
 '(foreground-color "#839496")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(linum-format " %7i " t)
 '(magit-diff-use-overlays nil)
 '(mode-require-final-newline t)
 '(op/personal-disqus-shortname "nil")
 '(op/personal-github-link "https://github.com/johnwalker")
 '(op/site-main-title "johnwalker")
 '(op/site-sub-title "Look, Leap")
 '(op/theme (quote mdo))
 '(op/theme-root-directory "/home/john/.emacs.d/org-page/themes/")
 '(paradox-github-token t)
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(safe-local-variable-values
   (quote
    ((eval when
	   (fboundp
	    (quote rainbow-mode))
	   (rainbow-mode 1)))))
 '(sp-hybrid-kill-excessive-whitespace nil)
 '(sp-successive-kill-preserve-whitespace 2)
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#A1EFE4" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#F92672" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#A6E22E"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#66D9EF" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#E6DB74"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#FD5FF0"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#FD971F" :weight bold))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
