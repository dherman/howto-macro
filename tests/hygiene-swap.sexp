(let-syntax ((swap! (syntax-rules ()
                      ((swap! x y)
                       (let ((tmp x))
                         (begin
                          (set! x y)
                          (set! y tmp)))))))
  (lambda (quux tmp)
    (begin
     (print quux)
     (print tmp)
     (swap! quux tmp)
     (print quux)
     (print tmp))))
