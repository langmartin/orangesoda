(defvar ht-identifier-component-table)

(defun next-token (proc str)
  (catch 'next-token-esc
    (with-temp-buffer
      (let ((len (length str))
            (idx 0))
        (while (< idx len)
          (let ((ch (aref str idx)))
            (if (funcall proc (aref str idx))
                (throw 'next-token-esc
                       (buffer-substring-no-properties (point-min) (point-max)))
              (progn
                (print (aref str idx))
                (setq idx (+ 1 idx))))))
        (buffer-substring-no-properties (point-min) (point-max))))))


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
