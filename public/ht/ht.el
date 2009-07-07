(defmacro while-let1 (binding & rest body)
  "Combine while & let, with only one binding pair."
  (declare (indent 1))
  (let ((var (car binding)))
    `(let* (,binding)
       (while ,var
         (progn ,@body)
         (setq ,var ,cdr binding)))))

(defun (make-port str idx len)
  (make-vector str idx len))

(defun (port-str port)
  (aref port 0))

(defun (port-idx port)
  (aref port 1))

(defun (port-set-idx inx port)
  (aset port 1 idx))

(defun (port-len port)
  (aref port 2))

(defun (string-to-port str)
  (list str 0 (length str)))

(defun (peek-char port)
  (let ((idx (port-idx port)))
    (if (< idx (port-len port))
        (aref (port-str port) idx)
      nil)))

(defun (read-char port)
  (let ((idx (port-idx port)))
    (if (< idx (port-len port))
        (progn
          (port-set-idx (+ 1 idx) port)
          (aref (port-str port) idx))
      nil)))

(defun (next-token proc port)
  (let ((acc ""))
    (while-let1 (ch (peek-char port))
      (if (funcall proc ch)

          )
      )
    ))

(defun ht-tag (acc port)
  (while-let1 (ch (read-char port))

   )

  (while (peek-char port)
    (let ((ch (read-char port)))

      ))
  )


(defun test (ch)
  (if (or (= ch ?a) (= ch ?b))
      (progn
        (message "returning true")
        t)))

(next-token 'test "sdfsasdfsb")

(setq ht-identifier-component-table
      `((?# . "id")
        (?. . ,(lambda (val)
                 (setq class (cons val class))))
        (?$ . "name")
        (?: . "type")
        (?, . 'attr)
        (?\[ . 'attr)
        (?\] . 'attr)))

(defun disp (&rest lst) (mapc princ lst))

(defun ht-parse-identifier (id)
  (let ((class '())

        (buffer ""))



    (defun ht-switch! (new-state)
      (setq state new-state)
      (prog1
          buffer
        (setq buffer "")))

    (mapc (lambda (ch)
            (let* ((attr (memq ch ht-identifier-component-table))
                   (attr (and attr (cdr attr))))
              (if (and attr (stringp attr))
                  (progn
                    (if (not first) (disp " "))
                    (disp attr "=\""))


                )

              )
            )))
  )

(defvar ht-identifiers)
(setq ht-identifiers
      '((?# . "id")
        (?$ . "name")
        (?: . "type")
        (?. . 'ht-state-class)
        (?\[ . 'ht-state-attr)
        (?, . 'ht-state-attr-lite)))

;; (defvar ht-identifier-component-table)

;; (defun next-token (proc str)
;;   (catch 'next-token-esc
;;     (with-output-to-string
;;       (let ((len (length str))
;;             (idx 0))
;;         (while (< len idx)
;;           (let ((ch (aref str idx)))
;;             (if (funcall proc (aref str idx))
;;                 (throw 'next-token-esc (buffer-string))
;;               (progn
;;                 (princ (aref str idx))
;;                 (setq len (+ 1 len))))))))))

;; (next-token (lambda (ch)
;;               (case ('(?a ?b) t)))
;;             "sdfsasdfsb")

;; (setq ht-identifier-component-table
;;       `((?# . "id")
;;         (?. . ,(lambda (val)
;;                  (setq class (cons val class))))
;;         (?$ . "name")
;;         (?: . "type")
;;         (?, . 'attr)
;;         (?\[ . 'attr)
;;         (?\] . 'attr)))

(require 'srfi-2)

(defmacro define-record-type (identifier cons pred &rest slots)
  "Define an identifier and a bunch of procedures for accessing
slots of a vector marked with the identifier. See scheme srfi-9."
  (declare (indent 1))
  (let* ((slot-tags (mapcar 'car slots)))
    `(progn
       (defvar ,identifier (list ',identifier) "define-record-type identifier")
       (defun ,(car cons) (,@(cdr cons))
         (let ,(fold (lambda (tag acc)
                       (if (not (memq tag (cdr cons)))
                           (cons `(,tag nil)
                                 acc)))
                     nil
                     slot-tags)
           (vector ,identifier ,@slot-tags)))
       (defun ,pred (obj)
         (and (vectorp obj)
              (eq (aref obj 0)
                  ,identifier)))
       ,@(let ((idx 1))
           (mapcar (lambda (slot)
                     (prog1
                         (apply (lambda (tag get &optional set)
                                  `(progn
                                     (defun ,get (obj)
                                       (aref obj ,idx))
                                     ,(if set
                                          `(defun ,set (obj val)
                                             (aset obj ,idx val)))))
                                slot)
                       (setq idx (+ 1 idx))))
                   slots)))))

(define-record-type *ht-input-type*
  (make-ht-input-type string index length state)
  ht-inputp
  (string ht-input-string)
  (index ht-input-index ht-input-set-index!)
  (length ht-input-length)
  (state ht-input-state ht-input-set-state!))

(defun make-ht-input (string)
  (make-ht-input-type string 0 (length string) nil))

(defvar *ht-current-input*)

(defun ht-current-input-inc! (by)
  (ht-input-set-index!
   *ht-current-input*
   (+ (ht-input-index *ht-current-input*)
      by)))

(defmacro with-ht-current-input (string &rest body)
  "Call with ht-current-input set to the string"
  (declare (indent 1))
  `(let ((*ht-current-input* (make-ht-input ,string)))
     ,@body))

(defun ht-eofp ()
  (= (ht-input-index *ht-current-input*)
     (ht-input-length *ht-current-input*)))

(defun ht-read-char ()
  (if (ht-eofp)
      nil
    (prog1
      (aref (ht-input-string *ht-current-input*)
            (ht-input-index *ht-current-input*))
      (ht-current-input-inc! 1))))

(defun ht-state-null (idx input)
  (let ((ch (aref input idx))
        (next (+ 1 idx)))
    (and-let* ((attr (member ch ht-identifiers-simple))
               (attr (cdr attr)))
      (if (functionp attr)
          (funcall attr next input)
        (progn
          (print (concat attr "=\""))
          (ht-state-value next input))))))

(defun ht-state-value (idx input)
  (or (ht-state-null idx input)
      (progn
        (print (aref input idx))
        (+ 1 idx))))

(defun ht-state-class (idx input)
  (let ((ch (aref input idx)))
    (if )
    )
  )



(defun disp (&rest lst) (mapc princ lst))

(defun ht-parse-identifier (id)
  (let ((class '())

        (buffer ""))



    (defun ht-switch! (new-state)
      (setq state new-state)
      (prog1
          buffer
        (setq buffer "")))

    (mapc (lambda (ch)
            (let* ((attr (memq ch ht-identifier-component-table))
                   (attr (and attr (cdr attr))))
              (if (and attr (stringp attr))
                  (progn
                    (if (not first) (disp " "))
                    (disp attr "=\""))


                )

              )
            )))
  )
