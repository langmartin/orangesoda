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

;;; I'm using fold from srfi-1, which is in this repository. Add
;;; public/emacs-lisp to your load-list in .emacs.
(require 'srfi-1)

(if (not (boundp 'org-orangesoda-publish-directory))
    (setq org-orangesoda-publish-directory "/orangesoda:/home/public/"))

(setq org-orangesoda-module-list
      '("squash" "jsmacs"))

(setq org-orangesoda-static-list
      '("css" "js" "scheme" "javascript" "emacs-lisp" "windows"))

(setq org-orangesoda-module-options
      (list
       :publishing-function 'org-publish-org-to-html
       :link-home "http://www.orangesoda.net/"
       :table-of-contents nil
       :section-numbers nil
       :todo-keywords nil
       :priority nil
       :author-info nil
       :auto-postamble t
       :style-include-default nil
       :style
       (concat
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/site.css\"/>\n"
        "<link rel=\"SHORTCUT ICON\" href=\"/favicon.ico\"/>")))

(setq org-orangesoda-static-options
      (list
       :publishing-function 'org-publish-attachment
       :base-extension
       (concat
        "js\\|el\\|scm\\|html\\|css\\|png\\|jpg\\|gif"
        "\\|ico\\|txt\\|ahk"
        "\\|ics"
        )))

(defun org-orangesoda-make-alist ()
  (let* ((src-root (concat default-directory "public/"))
         (module org-orangesoda-module-options)
         (static org-orangesoda-static-options)
         (cons (lambda (name src pub options tail)
                 (cons (append (list name
                                     :base-directory src
                                     :publishing-directory pub)
                               options)
                       tail)))
         (path (lambda (path pfix opts acc)
                 (funcall cons
                          (concat "orangesoda-" pfix "-" x)
                          (concat src-root x)
                          (concat org-orangesoda-publish-directory x)
                          opts
                          acc))))
    (fold (lambda (x acc)
            (funcall path x "static" static acc))
          (fold (lambda (x acc)
                  (funcall path x "static" static
                           (funcall path x "module" module acc)))
                (funcall cons "orangesoda-static"
                         src-root
                         org-orangesoda-publish-directory
                         static
                         (funcall cons "orangesoda-module"
                                  src-root
                                  org-orangesoda-publish-directory
                                  (append
                                   (list :auto-sitemap t
                                         :sitemap-filename "sitemap.org"
                                         :sitemap-title "Orangesoda Sitemap")
                                   module)
                                  nil))
                org-orangesoda-module-list)
          org-orangesoda-static-list)))

(setq org-publish-project-alist (org-orangesoda-make-alist))
(setq org-publish-project-alist
      (cons (list "orangesoda"
                  :components (mapcar 'car org-publish-project-alist))
            org-publish-project-alist))

(defun org-orangesoda-publish (&optional force)
  (interactive "P")
  (org-publish (assoc "orangesoda" org-publish-project-alist) force))

;;;; I was just playing with this, skip it for now.
;; (progn
;;   (require 'org-jekyll)
;;   (setq org-publish-jk "f:/tmp/blog")
;;   (setq org-publish-jk-blog (concat org-publish-jk "blog/"))
;;   (add-to-list 'org-publish-project-alist
;;                `("jk-org"
;;                  :base-directory "public"
;;                  :recursive t
;;                  :base-extension "org"
;;                  :publishing-directory ,org-publish-jk
;;                  :blog-publishing-directory ,org-publish-jk-blog
;;                  :site-root "http://juanreyero.com"
;;                  :jekyll-sanitize-permalinks t
;;                  :publishing-function org-publish-org-to-html
;;                  :section-numbers nil
;;                  :headline-levels 4
;;                  :table-of-contents t
;;                  :auto-index nil
;;                  :auto-preamble nil
;;                  :body-only t
;;                  :auto-postamble nil))
;;   (add-to-list 'org-publish-project-alist
;;                `("jk-img"
;;                  :base-directory "public"
;;                  :recursive t
;;                  :exclude "^publish"
;;                  :base-extension "jpg\\|gif\\|png"
;;                  :publishing-directory ,org-publish-jk
;;                  :publishing-function org-publish-attachment))
;;   (add-to-list 'org-publish-project-alist
;;                '("jk" :components ("jk-org"
;;                                    "jk-img")))
;;   )
