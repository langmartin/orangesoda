((lambda (a k)
   (cwcc
    (lambda (b)
      (set! a b))
    (lambda (b)
      (k b))))
 <uninitialized>
 (lambda (a)
   (a 5)))


(let ((a (lambda ()
           (call/cc
            (lambda (b)
              b)))))
  (a))

(let ((a (lambda (k)
           (lambda (b)
             (k b)))))
  (a display))
