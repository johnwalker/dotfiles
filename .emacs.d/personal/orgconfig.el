;; (add-to-list 'load-path "~/development/org-mode/lisp")
;; (add-to-list 'load-path "~/development/org-mode/contrib/lisp" t)

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

  ;; fontify code in code blocks
(setq org-src-fontify-natively t)

(use-package babel
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
		     (lisp . t)
		     (clojure . t)
		     (sh . t)
		     (org . t))))
	    (setq org-confirm-babel-evaluate nil)
	    (setq org-export-babel-evaluate nil)
	    (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
	    (setq org-src-fontify-natively t)))


(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

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

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/git/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/git/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/git/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/git/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/git/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/git/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/git/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(if (boundp 'org-user-agenda-files)
    (setq org-agenda-files org-user-agenda-files)
  (setq org-agenda-files (quote ("~/git/org"
				 ))))

(setq org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                            (org-agenda-skip-function 'bh/skip-stuck-projects)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

(org-clock-persistence-insinuate)
(setq org-clock-history-length 23)
(setq org-clock-in-resume t)
(setq org-clock-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-persist t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
(setq org-clock-report-include-clocking-task t)

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))

(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

(setq org-agenda-log-mode-items (quote (closed state)))

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(provide 'orgconfig)
