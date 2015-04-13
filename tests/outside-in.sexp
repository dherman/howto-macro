(let-syntax ((lambda1 (syntax-rules ()
                        ((lambda1 x e)
                         (lambda (x) e))))
             (lambda2 (syntax-rules ()
                        ((lambda2 x y e)
                         (lambda (x y) e)))))
  (lambda1 x
    (lambda2 y z
      (+ x y z))))
