;; (setq latex-run-command "xelatex")
;; (setq TeX-engine 'xetex)


(defvar tex-compile-commands
  '(("pdflatex --interaction=nonstopmode %f")))

(provide 'cfg-latex)
