#lang racket
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))
(define empty-tree '())
(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?
(define (make-leaf root) (make-tree root empty-tree empty-tree)) ; за удобство
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define t
  (make-tree 10
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(= k 0) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]))
(define head car)
(define tail cdr)

(define (tree-sum t)
  (define (loop q res)
    (cond [(null? q) res]
          [(empty-tree? (head q)) (loop (tail q) res)]
          [else (loop (cons (left-tree (head q))
                        (cons (right-tree (head q))
                          (tail q)))
                      (+ res (root-tree (head q))))]))
  (loop (list t) 0))

(define (bst-insert val t)
  (cond [(empty-tree? t) (make-leaf val)]
        [(< val (root-tree t))
         (make-tree (root-tree t)
                    (bst-insert val (left-tree t))
                    (right-tree t))]
        ;[(= val (root-tree t)) t]
        [else (make-tree (root-tree t)
                         (left-tree t)
                         (bst-insert val (right-tree t)))]))

(define (list->tree lst)
  (foldr bst-insert empty-tree lst))
(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t))
                    (list (root-tree t))
              (tree->list (right-tree t)))))

(define (sort lst)
  (tree->list (list->tree lst)))
(define (compose f g) (lambda (x) (f (g x))))
(define sort* (compose tree->list list->tree))
