(require 'org-publish)

(let ((top "~/Public/public/")
      (code-dir (lambda (dir)
                  `(,dir
                    :base-directory ,(concat "public/" dir)
                    :base-extension "js\\|el\\|scm"
                    :publishing-directory ,(concat top dir)
                    :publishing-function org-publish-attachment))))
  (setq
   org-publish-project-alist
   `(("orgfiles"
      :base-directory "public"
      :publishing-directory ,top
      :publishing-function org-publish-org-to-html
      :auto-index t
      :index-filename "sitemap.org"
      )

     ,(funcall code-dir "continuations")
     ,(funcall code-dir "ht")

     ("the-blag" :components ("orgfiles" "ht" "continuations"))
     )))
