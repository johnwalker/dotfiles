(require 'cider)

(mapc (lambda (mode)
        (add-hook (intern (format "%s-hook" (symbol-name mode))) 'smartparens-strict-mode))
      sp--lisp-modes)
(define-key lisp-mode-map (kbd ")") 'sp-up-sexp)


(add-hook 'clojure-mode-hook
          (lambda () (yafolding-mode)))

;; (require 'yasnippet)
;; (add-hook 'lisp-mode-hook 'yas-minor-mode-on)
;; (yas-load-directory "~/.emacs.d/snippets")

(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'subword-mode)

(setq cider-popup-stacktraces t
      cider-auto-select-error-buffer t
      nrepl-hide-special-buffers nil)
;; until bug is removed
(setq cider-auto-select-error-buffer nil)
(cljr-add-keybindings-with-prefix "C-c C-a")

(setq cider-repl-wrap-history t
      cider-repl-history-size 1300
      cider-repl-history-file "~/.ciderhistory"
      nrepl-hide-special-buffers t)

(define-key smartparens-strict-mode-map (kbd "M-(") 'sp-forward-barf-sexp)
(define-key smartparens-strict-mode-map (kbd "M-)") 'sp-forward-slurp-sexp)
(define-key smartparens-strict-mode-map (kbd "M-d") 'kill-word)
(define-key smartparens-strict-mode-map (kbd "s-s") 'sp-split-sexp)
(define-key smartparens-strict-mode-map (kbd "M-s") 'sp-splice-sexp)

(define-key lisp-mode-map            (kbd "C-<tab>") 'indent-sexp)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand-from-trigger-key)

(add-hook 'lisp-mode-hook 'idle-highlight-mode)

(require 'clojure-mode)
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
			       (clj-refactor-mode 1)
			       (cljr-add-keybindings-with-prefix "C-c C-r")))

(provide 'cfg-clojure)
