(require 'org-publish)

;;; So, it works to publish via tramp. Here are my dependencies:
;;;
;;; 1) In ~/.emacs, (setq tramp-default-method "sshx"). This is on windows,
;;;    it's possible/likely that this step is unnecessary on unix.
;;;
;;; 2) in ~/.ssh/config,
;;;    Host orangesoda
;;;    HostName ssh.phx.nearlyfreespeech.net
;;;    User langmartin_wibler
;;;
;;;    This layer of indirection takes care of the username variation, and
;;;    generally simplifies things.


(if (not (boundp 'orange-soda-publish-directory))
    (setq orange-soda-publish-directory "/orangesoda:/home/public/"))

(let* ((top orange-soda-publish-directory)
       (src (concat default-directory "public/"))
       (components '("orangesoda-org"))
       (orangesoda-dir
	(lambda (dir, ext)
	  (let ((name (concat "orangesoda-" dir)))
            (setq components (cons name components))
            `(,name
              :base-directory ,(concat src dir)
              :base-extension ,ext
              :publishing-directory ,(concat top dir)
              :publishing-function org-publish-attachment))))
       (code-dir
        (lambda (dir)
          (funcall orangesoda-dir dir "js\\\\|el\\\\|scm")))
       (static-media
        (lambda (dir)
          (funcall orangesoda-dir dir "js\\|css\\|png\\|jpg\\|gif"))))
  (setq
   org-publish-project-alist
   `(("orangesoda-org"
      :base-directory ,src
      :publishing-directory ,top
      :publishing-function org-publish-org-to-html

      :auto-index t
      :index-filename "sitemap.org"

      :table-of-contents nil
      :section-numbers nil
      :todo-keywords nil
      :priority nil
      :auto-postamble nil

      :style-include-default nil
      :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/site.css\" />")

     ,(funcall static-media "css")
     ,(funcall static-media "js")

     ,(funcall code-dir "ht")
     ,(funcall code-dir "scheme")
     ,(funcall code-dir "javascript")
     ,(funcall code-dir "emacs-lisp")

     ("orangesoda" :components ,components))))
