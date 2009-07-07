(defvar ht-identifier-component-table)

;; (defun next-token (proc str)
;;   (catch 'next-token-esc
;;     (with-temp-buffer
;;       (let ((len (length str))
;;             (idx 0))
;;         (while (< idx len)
;;           (let ((ch (aref str idx)))
;;             (if (funcall proc (aref str idx))
;;                 (throw 'next-token-esc
;;                        (buffer-substring-no-properties (point-min) (point-max)))
;;               (progn
;;                 (print (aref str idx))
;;                 (setq idx (+ 1 idx))))))
;;         (buffer-substring-no-properties (point-min) (point-max))))))
;; (defun next-token (proc str-or-pair)
;;   (let ((acc "")
;;         (str (if (stringp str-or-pair)
;;                  str-or-pair
;;                (cdr str-or-pair))))))

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
