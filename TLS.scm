(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)   ;; 是否是空的 是则真
     ((atom? (car l)) (lat? (cdr l))) ;; 不是空 第一个元素是不是原子 是则后面的是不是lat
     (else #f)))) ;; 都不是 则假

(define lat2?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     (else (cond
            ((atom? (car lat)) (lat2? (cdr lat)))
            (else #f))))))


(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat) (rember a (cdr lat)))))))))

(define firsts
  (lambda (lists)
    (cond
     ((null? lists) '())
     ())))
