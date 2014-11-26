;;; package --- Summary

;;; Commentary:

(setq debug-command "make -k")

(global-set-key (kbd "<f10>") '(lambda () (interactive) (shell-command debug-command)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/config")

;; Backup and save tweaks
(setq backup-directory-alist `(("." . "~/.saves"))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))

(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 500)

(require 'undo-tree)

(require 'cfg-ui)
(require 'cfg-org)
(require 'cfg-prodigy)
(require 'cfg-smartparens)
(require 'cfg-ido)
(require 'cfg-clojure)
(require 'cfg-eshell)
(require 'cl)

(load-theme 'solarized-light t)
(electric-indent-mode 1)
(setq confirm-nonexistent-file-or-buffer nil)

(add-hook 'c-mode-hook
          (lambda () (yafolding-mode)))

(add-hook 'haskell-mode-hook
          (lambda () (yafolding-mode)))

(setq inferior-lisp-program "sbcl")

(whole-line-or-region-mode 1)

(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile)
                                  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
                                  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
                                  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                                  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
                                  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

(eval-after-load 'haskell-cabal '(progn
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(defun intelligent-close ()
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      (if (> (length (visible-frame-list)) 1)
          (delete-frame (selected-frame))
        (save-buffers-kill-emacs))
    (delete-frame (selected-frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs lisp


(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

;; Smartparens
(define-key minibuffer-local-map        (kbd "M-o") 'minibuffer-complete-and-exit)
(define-key prodigy-mode-map            (kbd "s")   'prodigy-toggle)
(define-key smartparens-strict-mode-map (kbd "M-(") 'sp-forward-barf-sexp)
(define-key smartparens-strict-mode-map (kbd "M-)") 'sp-forward-slurp-sexp)
(define-key smartparens-strict-mode-map (kbd "M-d") 'kill-word)
(define-key smartparens-strict-mode-map (kbd "s-s") 'sp-split-sexp)
(define-key smartparens-strict-mode-map (kbd "M-s") 'sp-splice-sexp)

(global-set-key (kbd "<f12>") 'package-list-packages)
(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-+") 'push-mark-no-activate)
(global-set-key (kbd "C-.") 'delete-other-windows)
(global-set-key (kbd "C-x u") 'undo-tree-visualize)
(global-set-key (kbd "C-<down>") 'scroll-down-line)
(global-set-key (kbd "C-<f2>") 'org-capture)

(global-set-key (kbd "C-<up>") 'scroll-up-line)
(global-set-key (kbd "C-c C-y") 'browse-kill-ring)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c n") 'clean-up-buffer-or-region)
(global-set-key (kbd "C-h C-m") 'discover-my-major)
(global-set-key (kbd "C-x &") 'split-window-horizontally)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") 'intelligent-close)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x [") 'delete-other-windows)
(global-set-key (kbd "C-x g") 'ag)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-x {") 'split-window-vertically)
(global-set-key (kbd "M-+") 'jump-to-mark)
(global-set-key (kbd "M-<f1>") '(lambda () (interactive) (org-publish-project "blog")))
(global-set-key (kbd "M-<f2>") 'org-agenda)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-SPC") 'rectangle-mark-mode)
(global-set-key (kbd "s-p") 'prodigy)

(provide 'init)
;;; init.el ends here
(add-to-list 'load-path "/home/john/development/archive/org-mode/contrib/lisp/")
(eval-after-load 'ox '(progn
                        (require 'ox-koma-letter)
                        (add-to-list 'org-latex-classes
                                     '("my-letter"
                                       "\\documentclass\{scrlttr2\}
     \\usepackage[english]{babel}
     \\setkomavar{frombank}{(1234)\\,567\\,890}
     \[DEFAULT-PACKAGES]
     \[PACKAGES]
     \[EXTRA]"))
                        (setq org-koma-letter-default-class "my-letter")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(cider-auto-jump-to-error nil)
 '(cider-repl-history-file "~/.ciderhistory")
 '(cider-repl-history-size 1300)
 '(cider-repl-wrap-history t)
 '(custom-safe-themes
   (quote
    ("de66c72d07f5671ccf18901ab7873c94aa558d4231d678080ab194a2a95e6d91" "30a8a5a9099e000f5d4dbfb2d6706e0a94d56620320ce1071eede5481f77d312" "e4bc8563d7651b2fed20402fe37b7ab7cb72869f92a3e705907aaecc706117b5" "0c97dcff4ea6ac23af383e6153723a712c1de3a4b427e97d1e473504dbc2fe06" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3d003561784526d83d1dd187aecf4799c72af27046bc3aa2f6d95c64e5ee4746" "1934bf7e1713bf706a9cb36cc6a002741773aa42910ca429df194d007ee05c67" "26247bcb0b272ec9a5667a6b854125450c88a44248123a03d9f242fd5c6ec36f" "569dc84822fc0ac6025f50df56eeee0843bffdeceff2c1f1d3b87d4f7d9fa661" "c01f093ab78aad6ae2c27abc47519709c6b3aaa2c1e35c712d4dd81ff1df7e31" "a5beb9b1d6dc23dd8a3c204c159c9a5f1e0115ff14b5b8579d6f3ede4f3b3aee" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "efa048d8a9f9a8340a2f6382f3b8b8f4549cba38aa226803ff5b6a9b3a2d5f4b" "d5711dce56cd424110cf9daaa99c0495b4495c1b3f955a83f1cf04c4c73eba4f" "394504bd559027641b544952d6e9e1c6dcb306b4d1b2c4ad6b98d3e6b5459683" "0744f61189c62ed6d1f8fa69f6883d5772fe8577310b09e623c62c040f208cd4" "6c9ddb5e2ac58afb32358def7c68b6211f30dec8a92e44d2b9552141f76891b3" "6209442746f8ec6c24c4e4e8a8646b6324594308568f8582907d0f8f0260c3ae" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "3d568788393420c93d778df9a46c59b81dd5d9acabaf3b5962659bc0772012aa" "941bc214a26ed295e68bbeaadcd279475a3d6df06ae36b0b2872319d58b855f7" "46223bc978f9e7ab7f5b61d171da7ce98e69661158b75ed011603d3134fbad02" "ad9fc392386f4859d28fe4ef3803585b51557838dbc072762117adad37e83585" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" "8fd393097ac6eabfcb172f656d781866beec05f27920a0691e8772aa2cdc7132" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" "d25594db11e666e3d670c739266f62b206814e261e8548becef5258d6cfbd50b" "e35ef4f72931a774769da2b0c863e11d94e60a9ad97fb9734e8b28c7ee40f49b" default)))
 '(fci-rule-color "#383838")
 '(org-html-mathjax-options
   (quote
    ((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
     (scale "100")
     (align "center")
     (indent "2em")
     (mathml nil))))
 '(recentf-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
