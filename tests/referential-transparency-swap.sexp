(let-syntax ((swap! (syntax-rules ()
                      ((swap! x y)
                       (let ((tmp x))
                         (begin
                          (set! x y)
                          (set! y tmp)))))))
  (lambda (set!)
    (let ((x 1)
          (y 2))
      (begin
       (swap! x y)
       (print x)
       (print y)))))
