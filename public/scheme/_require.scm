;; Produce a COND-EXPAND form to meet REQUIREMENT.
(define (expand-requirement requirement #!key force)
  (let* ((path (requirement->path requirement))
         (feature (requirement-path->feature path))
         (header-file (string-append path "#.scm")))
    (receive (method method-path)
        (best-requirement path)
      (if (not (or method header-file))
          (error "Requirement does not exist." requirement)
          (cond-expand-requirement feature header-file method method-path force)))))

;; Mimic the way LOAD searches for the best candidate in the
;; filesytem.  Rather than loading anything, return (values METHOD
;; METHOD-PATH) where METHOD indicates how to process the METHOD-PATH.
(define (best-requirement path)
  (cond ((regular-file-exists? path)
         (values 'include path))
        ((not (string=? "" (path-extension path)))
         (values #f path))
        ((best-module path)
         => (lambda (module-path)
              (values 'load path)))
        ((best-source-file path)
         => (lambda (source-path)
              (values 'include source-path)))
        (else
         (values #f path))))

;; Find the most recent compiled module at PATH.
(define (best-module path)
  (let find-module ((version 1) (last-versioned-path #f))
    (let ((name (string-append path ".o" (number->string version 10))))
      (cond ((regular-file-exists? name)
             (find-module (+ version 1) name))
            (last-versioned-path
             last-versioned-path)
            (else
             #f)))))

;; Find the most relevant source file at PATH.
(define (best-source-file path)
  (let ((name (string-append path ".scm")))
    (and (regular-file-exists? name)
         name)))

;; Does PATH exists and is it a regular file?
(define (regular-file-exists? path)
    (and (file-exists? path)
         (eq? 'regular (file-info-type (file-info path)))))

;; Equivalent to (string-join requirement "/" 'strict-infix)
(define (requirement->path requirement)
  (if (null? requirement)
      (error "A requirement may not be empty.")
      (with-output-to-string
        ""
        (lambda ()
          (print (car requirement))
          (for-each (lambda (element)
                      (print "/" element))
                    (cdr requirement))))))

;; Create a COND-EXPAND feature from PATH.
(define (requirement-path->feature path)
  (string->symbol (string-append "require:/" path)))

;; Produce a COND-EXPAND form for FEATURE that possibly includes
;; HEADER-FILE and (METHOD-NAME METHOD-PATH).
(define (cond-expand-requirement feature header-file method-name method-path force)

  ;; PATH-EXPAND all paths since LOAD and INCLUDE do not resolve
  ;; relative paths the same way.

  (define (make-cond-expand)
    (if force
        (body)
        `(cond-expand
          ((not ,feature) ,(body))
          (else ,(header)))))

  (define (body)
    `(begin
       ,(library)
       ,(requirement-met!)))

  (define (header)
    (maybe-load-source header-file))

  (define (library)
    (cond (method-name
           (method))
          (header-file
           (header))
          (else
           (error "cond-expand-requirement: no method-name or header-file."))))

  (define (requirement-met!)
    `(define-cond-expand-feature ,feature))

  (define (method)
    `(begin
       (,method-name ,(path-expand method-path))
       ,(maybe-header)))

  ;; Assume that included source files include their own header.
  (define (maybe-header)
    (if (not (eq? method-name 'include))
        (header)
        `(begin)))

  (define (maybe-load-source path)
    (maybe-include (path-expand path)))

  (define (maybe-include path)
    (if (regular-file-exists? path)
        `(include path)
        `(begin)))

  (make-cond-expand))
