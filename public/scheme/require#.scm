;;;; Require -- a load-once special form for Gambit-C.
;;;
;;; The REQUIRE special form includes source files or loads header
;;; files and modules.  It accepts a sequence of requirement
;;; specifications that identify libraries.
;;;
;;;   (require <requirement> ...)
;;;
;;; A <requirement> has the form:
;;;
;;;   (identifier-1 identifier-2 ...)
;;;
;;; For example:
;;;
;;;   (require (srfi 1) (lib util))
;;;
;;; The REQUIRE form uses COND-EXPAND to prevent a library from being
;;; included more than once.  Use REQUIRE! to force a library to be
;;; included again.
;;;
;;; This implementation is not associated with a LIBRARY definition,
;;; so the identifiers in a <requirement> are resolved to library
;;; implementations by searching in the filesystem.  The requirement
;;; is converted to a path which is assumed to be relative to the
;;; CURRENT-DIRECTORY.
;;;
;;;    1. Convert the requirement to a path: (srfi 1) => "srfi/1"
;;;
;;;    2. If the file "srfi/1" exists: INCLUDE it and stop.
;;;
;;;    3. If the file "srfi/1.o?" exists: LOAD "srfi/1.o?", try to
;;;       INCLUDE "srfi/1#.scm" and stop.
;;;
;;;    4. Finally, if the file "srfi/1.scm" exists, INCLUDE it and assume
;;;       that it includes "srfi/1#.scm" itself if it exists.
;;;
;;; All relative paths are expanded with PATH-EXPAND before being
;;; given to INCLUDE or LOAD because Gambit-C's normal behavior is to
;;; INCLUDE files relative to the current source file rather than the
;;; CURRENT-DIRECTORY.
;;;
;;; An easy way to integrate REQUIRE into a project is to include this
;;; file from `.gambcini' or '~/.gambcini'.  For more details, see
;;; <http://www.iro.umontreal.ca/~gambit/doc/gambit-c.html#Customization-1>.

(define-macro (require . requirements)
  `(expand-requirements ,requirements))

(define-macro (require! . requirements)
  `(expand-requirements ,requirements force: #t))

(define-macro (expand-requirements requirements . options)
  (include "_require.scm")
  `(begin
     ,@(map (lambda (requirement)
              (apply expand-requirement requirement options))
            requirements)))