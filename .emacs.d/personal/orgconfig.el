(add-to-list 'load-path "~/development/org-mode/lisp")
(add-to-list 'load-path "~/development/org-mode/contrib/lisp" t)

(require 'ox-publish)
(require 'htmlize)
(setq org-html-htmlize-output-type 'inline-css)
(setq org-html-validation-link nil)

(setq org-publish-project-alist
      '(("blog" :components ("orgblog" "images" "feed-clojure" "feed-blog" "other" "js"))
	("orgblog"
	 :auto-sitemap nil
	 :author "John Walker"
	 :sitemap-title "John's Blog"
	 :auto-postamble nil
	 :htmlized-source t
	 :base-directory "~/documents/blorg"
	 :base-extension "org"
	 :export-author-info t
	 :export-creator-info nil
	 :sitemap-sort-files anti-chronologically
	 :publishing-directory "~/documents/blog"
	 :publishing-function my-org-publish-org-to-html
	 :exclude "~/documents/blorg/css feed.org feed-clojure.org" 
	 :recursive t
	 :headline-levels 5 
	 :html-preamble
	 " <a href=\"#skip\" class=\"offscreen\">Skip Content</a> 
<div class=\"heading\">
      <nav>
        <ul>
          <li><a class=\"loc\" href=\"/\">johnwalker.github.io</a></li>
          <li><a class=\"loc\" href=\"https://github.com/johnwalker\">github</a></li>
          <li><a class=\"loc\" href=\"https://twitter.com/johnwalker301\">twitter</a></li>
        </ul>
      </nav>
</div>"
	 :section-numbers nil
	 :html-head-include-default-style nil
	 :description "Thoughts on Software development, Natural Language Processing and Networks."
	 :html-head-include-scripts nil
	 :html-head-extra
	 "
<link href='http://fonts.googleapis.com/css?family=Molengo' rel='stylesheet' type='text/css'>
<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"http://johnwalker.github.io\"
                title=\"RSS feed for johnwalker.github.io\">"
	 :with-toc nil)
	("feed-clojure"
	 :base-directory "~/documents/blorg"	 
	 :base-extension "org"
	 :author "John Walker" 
	 :webmaster "john.lou.walker@gmail.com"
	 :publishing-directory "~/documents/blog"	 
	 :publishing-function org-rss-publish-to-rss
	 :html-link-home "http://johnwalker.github.io/"
	 :html-link-use-abs-url t
	 :author "John Walker"
	 :with-author "John Walker"
	 :exclude ".*"
	 :include ("feed-clojure.org"))
	("feed-blog"
	 :base-directory "~/documents/blorg"	 
	 :base-extension "org"
	 :author "John Walker" 
	 :webmaster "john.lou.walker@gmail.com"
	 :with-author "John Walker"	 
	 :publishing-directory "~/documents/blog"	 
	 :publishing-function org-rss-publish-to-rss
	 :html-link-home "http://johnwalker.github.io/"
	 :html-link-use-abs-url t
	 :exclude ".*"
	 :include ("feed.org"))
	("images"
	 :author "John Walker"	 
	 :base-directory "~/documents/blorg/images"
	 :base-extension "jpg\\|gif\\|png"
	 :exclude "~/development/blog/css"
	 :publishing-directory "~/documents/blog/images"
	 :publishing-function org-publish-attachment)
	("other"
	 :author "John Walker"	 
	 :base-directory "~/documents/blorg/css"
	 :base-extension "css"
	 :publishing-directory "~/documents/blog/css"
	 :publishing-function org-publish-attachment)
	("js"
	 :author "John Walker"	 
	 :base-directory "~/documents/blorg/js"
	 :base-extension "js"
	 :publishing-directory "~/documents/blog/js"
	 :publishing-function org-publish-attachment)))

(defun my-org-html-postamble (plist)
  (format "<p class=\"postamble\">Last update : %s </p>" (format-time-string "%d %b %Y")))

(setq org-html-postamble 'my-org-html-postamble)

(defun org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let ((decl (or (and (stringp org-html-xml-declaration)
			  org-html-xml-declaration)
		     (cdr (assoc (plist-get info :html-extension)
				 org-html-xml-declaration))
		     (cdr (assoc "html" org-html-xml-declaration))

		     "")))
       (when (not (or (eq nil decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
				  (fboundp 'coding-system-get)
				  (coding-system-get org-html-coding-system 'mime-charset))
			     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html" 
	   (when (org-html-xhtml-p info)
	     (format
	      " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
	      (plist-get info :language) (plist-get info :language)))
	   ">\n")			
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info) 
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format org-html-home/up-format
	       (or link-up link-home)
	       (or link-home link-up))))
   (org-html--build-pre/postamble 'preamble info)
   (format "<%s id=\"%s\">\n"
	   (nth 1 (assq 'content org-html-divs))
	   (nth 2 (assq 'content org-html-divs)))
   "<a href=\"#skip\" class=\"offscreen\">Skip Content</a> "
   contents
   (format "</%s>\n"
	   (nth 1 (assq 'content org-html-divs)))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Closing document.
   "</body>\n</html>"))

(defun my-org-publish-org-to-html (plist filename pub-dir)
  (org-publish-org-to 'html filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension "html"))
		      plist pub-dir))

(require 'ob)
(require 'ob-clojure)

(setq org-babel-clojure-backend 'cider)

(use-package ox-rss)
(setq org-export-htmlize-output-type 'css)


(use-package babel
  :defer t
  :ensure babel
  :config (progn
	    (setq org-ditaa-jar-path "~/toolbox/ditaa0_9.jar")
	    (setq org-plantuml-jar-path "~/toolbox/plantuml.jar")
	    (setq org-babel-results-keyword "results")
	    (setq org-export-latex-listings t)            
	    
	    (org-babel-do-load-languages
	     (quote org-babel-load-languages)
	     (quote ((emacs-lisp . t)
		     ;; (dot . t)		
		     (ditaa . t)
		     (java . t)
		     (gnuplot . t)
		     (clojure . t)
		     (sh . t)
		     (org . t))))
	    (setq org-confirm-babel-evaluate nil)
	    (setq org-export-babel-evaluate nil)
	    (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
	    (setq org-src-fontify-natively t)))


;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; (setq org-latex-listings 'minted)

(require 'tex)
(use-package ox-latex
  :defer t
  :config (progn
	    (setq latex-run-command "pdflatex")
	    (TeX-global-PDF-mode t)
	    (setq TeX-PDF-mode t)
	    (setq TeX-auto-save t)
	    (setq TeX-parse-self t)
	    (setq-default TeX-master nil)
	    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
	    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
	    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
	    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
	    (setq reftex-plug-into-AUCTeX t)
	    (setq org-latex-pdf-process
		  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
		    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
		    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))))

(provide 'orgconfig)
