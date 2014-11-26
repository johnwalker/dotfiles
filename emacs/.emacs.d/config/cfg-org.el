(require 'ox-publish)

(defadvice org-export-grab-title-from-buffer
    (around org-export-grab-title-from-buffer-disable activate))

(defun my-org-publish-org-to-html (plist filename pub-dir)
  (org-publish-org-to 'html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension "html"))
                      plist pub-dir))

(add-hook 'org-mode-hook '(lambda ()
                            (local-set-key (kbd "<f4>")
                                           'org-edit-src-code)))

(require 'ox)
(require 'ox-publish)
(require 'htmlize)
(setq org-html-htmlize-output-type 'inline-css)
(setq org-html-validation-link nil)

(setq org-publish-project-alist
      '(("blog" :components ("orgblog"))
        ("orgblog"
         :auto-sitemap nil
         :author "John Walker"
         :sitemap-title "John's Blog"
         :auto-postamble nil
         :htmlized-source nil
         :base-directory "~/github.com/blog"
         :base-extension "org"
         :export-author-info t
         :export-creator-info nil
         :html-html5-fancy t
         :html-doctype "html5"
	 :html-head-extra
	 "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/main.css\">
	 <link rel=\"shortcut icon\" href=\"/favicon.ico\" type=\"image/x-icon\" />
	 <link rel=\"shortcut icon\" href=\"/favicon.png\" type=\"image/png\" />"
         :html-preamble
         "<nav>
            <li><a class=\"loc\" href=\"/\">index</a></li>
          </nav>"
         :html-postamble
         ;; "<footer><ul><li><a class=\"loc\" href=\"https://github.com/johnwalker\">github</a></li></ul></footer>"
	 "<footer><ul><li><a class=\"loc\" href=\"https://twitter.com/johnwalkerio\">twitter</a></li><li><a class=\"loc\" href=\"https://github.com/johnwalker\">github</a></li></ul></footer>"
         :sitemap-sort-files anti-chronologically
         :publishing-directory "~/github.com/blog/out"
         :publishing-function my-org-publish-org-to-html
         :with-drawers t
         :with-special-strings t
         :publishing-function my-org-publish-org-to-html
         :exclude "~/documents/blorg/css feed.org feed-clojure.org"
         :recursive t
         :headline-levels 5
         :section-numbers nil
         :html-head-include-default-style nil
         :description "John Walker's blog"
         :html-head-include-scripts nil
         :with-toc t)))



(defun my-org-html-postamble (plist)
  (format "<p class=\"postamble\">Last update : %s </p>" (format-time-string "%Y %b %d")))
(setq org-html-postamble 'my-org-html-postamble)
(defun org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
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
   ;; "<a href=\"#skip\" class=\"offscreen\">Skip Content</a> "
   contents
   (format "</%s>\n"
	   (nth 1 (assq 'content org-html-divs)))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Closing document.
   "</body>\n</html>"))

;; Bug in org mode? ???
;; http://wenshanren.org/?p=781
(defun org-font-lock-ensure ()
  (font-lock-fontify-buffer))

(defun my-org-publish-org-to-html (plist filename pub-dir)
  (org-publish-org-to 'html filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension "html"))
		      plist pub-dir))
(require 'ob)
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(setq org-export-htmlize-output-type 'css)
(setq org-src-fontify-natively t)

(require 'babel)
(setq org-babel-results-keyword "results")
(setq org-export-latex-listings t)
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
	 (ditaa . t)
	 (java . t)
	 (gnuplot . t)
	 (lisp . t)
	 (clojure . t)
	 (sh . t)
	 (org . t))))

(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate nil)
(setq org-src-fontify-natively t)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(require 'ox-latex)
(setq latex-run-command "pdflatex")

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/git/org/agenda/refile.org")
	       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
	      ("r" "respond" entry (file "~/git/org/agenda/refile.org")
	       "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
	      ("n" "note" entry (file "~/git/org/agenda/refile.org")
	       "* %? :NOTE:\n%U\n%a\n")
	      ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
	       "* %?\n%U\n"))))

(if (boundp 'org-user-agenda-files)
    (setq org-agenda-files org-user-agenda-files)
  (setq org-agenda-files (quote ("~/git/org/agenda"))))

(provide 'orgconfig)

(provide 'cfg-org)
