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
       :link-home "http://orangesoda.net/"
       :table-of-contents nil
       :section-numbers nil
       :todo-keywords nil
       :priority nil
       :auto-postamble nil
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
        "\\|ico\\|txt\\|ahk")))

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
                                   (list :auto-index t
                                         :index-filename "sitemap.org"
                                         :index-title "Orangesoda Sitemap")
                                   module)
                                  nil))
                org-orangesoda-module-list)
          org-orangesoda-static-list)))

(setq org-publish-project-alist (org-orangesoda-make-alist))
(setq org-publish-project-alist
      (cons (list "orangesoda"
                  :components (mapcar 'car org-publish-project-alist))
            org-publish-project-alist))
