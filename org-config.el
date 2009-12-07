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

(defun org-orangesoda-static (dir)
  (let ((src (concat default-directory "public/"))
        (name (concat "orangesoda-static-" dir)))
    `(,name
      :base-directory ,(concat src dir)
      :base-extension "js\\|el\\|scm\\|html\\|css\\|png\\|jpg\\|gif"
      :publishing-directory ,(concat orange-soda-publish-directory dir)
      :publishing-function org-publish-attachment)))

(defun org-orangesoda-submodule (dir)
  (let* ((src (concat default-directory "public/"))
         (pub orange-soda-publish-directory)
         (base (if (string= "" dir) src (concat src dir)))
         (publish (if (string= "" dir) pub (concat pub dir)))
         (name (if (string= "" dir) "orangesoda.net"
                 (concat "orangesoda-" dir))))
    (let ((module
           `(,name
             :base-directory ,base
             :publishing-directory ,publish
             :publishing-function org-publish-org-to-html
             :link-home "http://orangesoda.net/"
             :table-of-contents nil
             :section-numbers nil
             :todo-keywords nil
             :priority nil
             :auto-postamble nil
             :style-include-default nil
             :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/site.css\" />")))
      (if (string= "" dir)
          (setq module
                (append module
                        '(:auto-index t :index-filename "sitemap.org"))))
      module)))

(defun org-orangesoda-build ()
  (let ((project '()))
    (setq project
          (append
           (mapcar 'org-orangesoda-submodule
                   '("" "squash" "jsmacs"))
           (mapcar 'org-orangesoda-static
                   '("css"
                     "js"
                     "scheme"
                     "javascript"
                     "emacs-lisp"
                     "squash" "squash/js"
                     "jsmacs"
                     ))))
    (setq project
          (cons (cons "orangesoda"
                      (list :components
                            (mapcar 'car project)))
                project))
    project))

(defun org-orangesoda-clobber ()
  (interactive)
  (setq org-publish-project-alist (org-orangesoda-build)))

(org-orangesoda-clobber)
