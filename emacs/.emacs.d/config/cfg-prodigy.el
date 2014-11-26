(require 'prodigy)

(prodigy-define-service
  :name "Blog 1620"
  :command "python"
  :args '("-m" "http.server" "1620")
  :cwd "~/github.com/blog/out"
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

(provide 'cfg-prodigy)
