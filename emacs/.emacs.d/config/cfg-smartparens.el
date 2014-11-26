(require 'smartparens)
(require 'smartparens-config)
(sp-pair "'" nil :actions :rem)
(sp-pair "`" nil :actions :rem)
(smartparens-global-mode t)
(add-hook 'clojure-mode-hook 'turn-on-smartparens-strict-mode)
(define-key lisp-mode-map (kbd ")") 'sp-up-sexp)

(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))

(provide 'cfg-smartparens)
