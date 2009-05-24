(require 'org-publish)

(setq
 the-blag-publishing
 `("the-blag"
   :base-directory "blog"
   :publishing-directory "~/Public/blog"
   :publishing-function 'org-publish-org-to-html
   :auto-index t
   :index-filename "sitemap.html"
   ))

(add-to-list 'org-publish-project-alist
             the-blag-publishing)
