(defvar ht-identifier-component-table)

(defun next-token (proc str)
  (catch 'next-token-esc
    (with-output-to-string
      (let ((len (length str))
            (idx 0))
        (while (< len idx)
          (let ((ch (aref str idx)))
            (if (funcall proc (aref str idx))
                (throw 'next-token-esc (buffer-string))
              (progn
                (princ (aref str idx))
                (setq len (+ 1 len))))))))))

(next-token (lambda (ch)
              (case ('(?a ?b) t)))
            "sdfsasdfsb")

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
