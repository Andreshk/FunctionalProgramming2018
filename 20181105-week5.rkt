#lang racket
(define head car)
(define tail cdr)

; Намиране на дължината на списък - рекурсивно и итеративно
(define (length* lst)
  (if (null? lst) 0
      (+ 1 (length* (tail lst)))))
(define (length-i lst)
  (define (loop lst res)
    (if (null? lst) res
        (loop (tail lst) (+ res 1))))
  (loop lst 0))

; Обръщане на списък, също итеративно и рекурсивно
; (за демонстрация на предимствата на двата метода)
(define (reverse lst)
  (if (null? lst) '()
      (append (reverse (tail lst))
              (list (head lst)))))

(define (reverse-i lst)
  (define (loop lst res)
    (if (null? lst) res
        (loop (tail lst) (cons (head lst) res))))
  (loop lst '()))

; Варианти с foldr:
(define (length** lst)
  (foldr (lambda (el result) (+ 1 result)) 0 lst))

(define (reverse** lst)
  (foldr (lambda (el result) (append result (list el))) '() lst))

(define (map** f lst)
  (foldr (lambda (el result) (cons (f el) result)) '() lst))

(define (filter** p? lst)
  (foldr (lambda (el result) (if (p? el)
                                 (cons el result)
                                 result))
         '() lst))

; Взимане на n-ти елемент на списък
(define (nth n lst)
  (cond [(null? lst) #f]
        [(= n 0) (head lst)]
        [else (nth (- n 1) (tail lst))]))

; Списък от цифрите на число
(define (digit-list n)
  (define (loop n lst)
    (if (= n 0) lst
        (loop (quotient n 10)
              (cons (remainder n 10) lst))))
  (if (= n 0) '(0)
      (loop n '())))

; Взимане на първите n елемента от списък
(define (take n lst)
  (cond [(null? lst) '()]
        [(= n 0) '()]
        [else (cons (head lst)
                    (take (- n 1) (tail lst)))]))

; Премахване на първите n елемента от списък
(define (drop n lst)
  (cond [(null? lst) '()]
        [(= n 0) lst]
        [else (drop (- n 1) (tail lst))]))

; Не работи когато (> n (length lst)) !!!
(define (take* n lst)
  (map (lambda (i) (nth i lst))
       (range 0 n))) ; вградената функция range

; Проверка дали всеки/някой елемент на списък изпълнява предикат
(define (all? p? lst)
  (cond [(null? lst) #t]
        [(not (p? (head lst))) #f]
        [else (all? p? (tail lst))]))

;(define (all? p? lst)
;  (foldr (lambda (el result) (and (p? el) result))
;         #t
;         lst))

(define (any? p? lst)
  (not (all? (lambda (x) (not (p? x))) lst))) ; стандартно

; Зад.6 & 7
(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (head lst1) (head lst2))
            (zipWith f (tail lst1) (tail lst2)))))

(define (zip lst1 lst2)
  (zipWith cons lst1 lst2))

; Проверка дали списък е сортиран
(define (sorted? lst)
  (cond [(null? lst) #t]
        [(null? (tail lst)) #t]
        [(< (head lst) (head (tail lst))) (sorted? (tail lst))]
        [else #f]))

; Взимане на уникалните стойности от списък
(define (uniques lst)
  (if (null? lst) '()
      (cons (head lst)
            (uniques (filter (lambda (x) (not (equal? x (head lst))))
                             (tail lst))))))

; Вмъкване на елемент на правилното място в сортиран списък
(define (insert val lst)
  (cond [(null? lst) (list val)]
        [(< val (head lst)) (cons val lst)]
        [else (cons (head lst)
                    (insert val (tail lst)))]))

; Сортиране чрез вмъкване:
(define (insertion-sort lst)
  (foldr insert '() lst)) ; simple