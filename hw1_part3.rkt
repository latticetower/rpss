#lang racket

(define one-itemo
  (lambda (x s out)
    (conde [(== s '())(== '() out)]
           [(=/= s '())
            (fresh (first rest result)
                   (== (cons first rest) s)
                   (== (cons (cons x first) result) out)
                   (one-itemo x rest result)
            )
            ])))

(define assqo
  (lambda (x ls out)
    (conde [(=/= ls '())
            (fresh (ff fr rest)
                   (== (cons (cons ff fr) rest) ls)
                   (== ff x)(== (cons ff fr) out))]
           [(=/= ls '())
            (fresh (ff fr rest)
                   (=/= (cons (cons ff fr) rest) ls)
                   (== (cons ff fr) ls)
                   (assqo x fr out)
                   )]
     )))


(define multi-rembero
  (lambda (e l out)
    (conde [(== l '())(== '() out)]
           [(fresh (rest)
                   (== (cons e rest) l)
                   (multi-rembero e rest out)
                   )]
           [(fresh (first rest result)
                   (== (cons first rest) l)
                   (=/= first e)
                   (== (cons first result) out)
                   (multi-rembero e rest result)
                   )])))