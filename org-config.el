(require 'org-publish)

(let* ((top "~/Sites/orangesoda/")
       (components '("orgfiles"))

       (code-dir
	(lambda (dir)
	  (setq components (cons dir components))
	  `(,dir
	    :base-directory ,(concat "public/" dir)
	    :base-extension "js\\|el\\|scm"
	    :publishing-directory ,(concat top dir)
	    :publishing-function org-publish-attachment)))

       (static-media
	(lambda (dir)
	  (setq components (cons dir components))
	  `(,dir
	    :base-directory ,(concat "public/" dir)
	    :base-extension "js\\|css\\|png\\|jpg\\|gif"
	    :publishing-directory ,(concat top dir)
	    :publishing-function org-publish-attachment))))

  (setq
   org-publish-project-alist
   `(("orangesoda"
      :base-directory "public"
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

     ("the-blag" :components ,components)))
  )


