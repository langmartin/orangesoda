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

(defmacro and-let1 (binding &rest body)
  "Bind one pair, and execute the body if the value of
the binding is true. Use and-let* instead."
  (declare (indent 1))
  (let ((sym (car binding))
        (exp (cdr binding)))
    `(let ((,sym ,@exp))
       (if ,sym
           (progn
             ,@body)))))

(defmacro and-let* (bindings &rest body)
  "Bind variables like let*, but ensuring that each value is true
  in sequences. As values are true, continue to bind and then
  execute the body. See scheme srfi-2."
  (declare (indent 1))
  (if (= (length bindings) 1)
      `(and-let1 ,(car bindings) ,@body)
    `(and-let1 ,(car bindings)
       (and-let* ,(cdr bindings)
         ,@body))))

(defmacro define-record-type (identifier cons pred &rest slots)
  "Define an identifier and a bunch of procedures for accessing
slots of a vector marked with the identifier. See scheme srfi-9."
  (declare (indent 1))
  (let* ((slot-tags (mapcar 'car slots)))
    `(progn
       (defvar ,identifier nil "define-record-type identifier")
       (setq ,identifier (list ',identifier))
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
