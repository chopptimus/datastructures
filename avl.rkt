; A node is a list of four elements: value, height, left and right
(define value car)
(define left caddr)
(define right cadddr)

(define (height bst)
   (if (null? bst)
       0
       (cadr bst)))

(define (insert* x bst)
  (cond ((null? bst) (new-node x null null))
        ((<= x (value bst)) (new-node (value bst)
                                      (insert* x (left bst))
                                      (right bst)))
        (else (new-node (value bst)
                        (left bst)
                        (insert* x (right bst))))))

(define (new-node x left right)
  (list x (+ (max (height left) (height right)) 1) left right))

(define (left-rotate bst)
  (let ([new-child (new-node (value bst)
                             (left bst)
                             (left (right bst)))])
    (new-node (value (right bst))
              new-child
              (right (right bst)))))

(define (right-rotate bst)
  (let ([new-child (new-node (value bst)
                             (right (left bst))
                             (right bst))])
    (new-node (value (left bst))
              (left (left bst))
              new-child)))

(define (height-difference avl)
  (- (height (left avl)) (height (right avl))))

(define (balanced-left-heavy? avl)
  (= (height-difference avl) 1))

(define (balanced-right-heavy? avl)
  (= (height-difference avl) -1))

(define (left-heavy? avl)
  (= (height-difference avl) 2))

(define (right-heavy? avl)
  (= (height-difference avl) -2))

(define fix-avl-violation-right-1 left-rotate)

(define (fix-avl-violation-right-2 avl)
  (let ([new-root (new-node (value avl)
                            (left avl)
                            (right-rotate (right avl)))])
    (left-rotate new-root)))

(define fix-avl-violation-left-1 right-rotate)

(define (fix-avl-violation-left-2 avl)
  (let ([new-root (new-node (value avl)
                            (left-rotate (left avl))
                            (right avl))])
    (right-rotate new-root)))

(define (fix-avl-property avl)
  (if (null? avl)
      null
      (let ([fixed-left (fix-avl-property (left avl))]
            [fixed-right (fix-avl-property (right avl))])
        (fix-avl-node (new-node (value avl)
                                fixed-left
                                fixed-right)))))

(define (fix-avl-node avl)
  (cond ((right-heavy? avl) (if (balanced-left-heavy? (right avl))
                                (fix-avl-violation-right-2 avl)
                                (fix-avl-violation-right-1 avl)))
        ((left-heavy? avl) (if (balanced-right-heavy? (left avl))
                               (fix-avl-violation-left-2 avl)
                               (fix-avl-violation-left-1 avl)))
        (else avl)))

(define (insert x avl)
  (let ([avl (insert* x avl)])
    (fix-avl-property avl)))
