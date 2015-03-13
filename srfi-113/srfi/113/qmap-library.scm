;; qmap-library.scm -- library of quantity map procedures for sets and bags
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-113/blob/master/LICENSE

;; This file contains common procedures that implement domain logic behind sets
;; and bags. These are 'library' procedures in the sense that they themselves
;; are implemented in terms of 'primitive' procedures exposed by "qmaps.scm".
;;
;; Qmaps stand for quantity maps, mutable (element -> quantity) mappings. They
;; give us some abstraction layer over hash tables or whatever is actually used
;; to implement qmaps.
;;
;; Most of these procedures have side effects. Generally, the mutated qmap goes
;; as the first argument (and is called 'dst-qmap). 'src-qmap' is not mutated.
;; There are exceptions though.


;; Adding and removing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are used to enforce the invariant 'elements of sets are either absent
;; in the qmap, or have quantity equal to 1'. Whenever you see 'qmap-add!' it
;; means you should pass one of these procedures there.

(define (qmap-add/set! qmap element quantity)
  (qmap-set! qmap element quantity))

(define (qmap-add/bag! qmap element quantity)
  (qmap-inc! qmap element quantity))

(define (qmap-adjoin! qmap elements qmap-add!)
  (for-each
    (lambda (element) (qmap-add! qmap element 1))
    elements))

(define (qmap-delete! qmap elements)
  (for-each
    (lambda (element) (qmap-rem! qmap element))
    elements))

(define (qmap-unfold! qmap stop? mapper successor seed qmap-add!)
  (let loop ((value seed))
    (unless (stop? value)
      (qmap-add! qmap (mapper value) 1)
      (loop (successor value)))))


;; Searching and testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qmap-find qmap predicate failure)
  (qmap-for-each/stop qmap
    (lambda (element quantity stop)
      (when (predicate element)
        (stop element)))
    (lambda () (failure))))

(define (qmap-any? qmap predicate)
  (qmap-for-each/stop qmap
    (lambda (element quantity stop)
      (when (predicate element)
        (stop #t)))
    (lambda () #f)))

(define (qmap-every? qmap predicate)
  (qmap-for-each/stop qmap
    (lambda (element quantity stop)
      (unless (predicate element)
        (stop #f)))
    (lambda () #t)))


;; Mapping and folding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qmap-count qmap predicate)
  (qmap-fold qmap 0
    (lambda (element quantity count)
      (if (predicate element)
          (+ quantity count)
          count))))

(define (qmap-map! dst-qmap src-qmap proc qmap-add!)
  (qmap-for-each src-qmap
    (lambda (element quantity)
      (qmap-add! dst-qmap (proc element) quantity))))

(define (qmap-product! qmap n)
  (cond ((<= n 0) (qmap-clear! qmap)) ; easy mode, can be optimized
    (else
      (qmap-for-each qmap
        (lambda (element quantity)
          (qmap-set! qmap element (* n quantity)))))))

;; These procedures iterate over qmap's elements taking into account their
;; quantity (i.e., as if equivalent elements were distinct). qmap-*/set are
;; optimized variants of qmap-*/bag procedures that do not have an inner loop
;; (they assume quantity to be one, as in sets).

(define (qmap-for-each/set qmap proc)
  (qmap-for-each qmap
    (lambda (element quantity)
      (proc element))))

(define (qmap-for-each/bag qmap proc)
  (qmap-for-each qmap
    (lambda (element quantity)
      (let loop ((quantity quantity))
        (unless (= 0 quantity)
          (proc element)
          (loop (- quantity 1)))))))

(define (qmap-fold/set qmap knil kons)
  (qmap-fold qmap knil
    (lambda (element quantity value)
      (kons element value))))

(define (qmap-fold/bag qmap knil kons)
  (qmap-fold qmap knil
    (lambda (element quantity value)
      (let loop ((value value) (quantity quantity))
        (if (= 0 quantity) value
            (loop (kons element value) (- quantity 1)))))))


;; Filtering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qmap-filter! dst-qmap predicate src-qmap)
  (qmap-for-each src-qmap
    (lambda (element quantity)
      (when (predicate element)
        (qmap-set! dst-qmap element quantity)))))

(define (qmap-remove! dst-qmap predicate src-qmap)
  (qmap-for-each src-qmap
    (lambda (element quantity)
      (when (predicate element)
        (qmap-rem! dst-qmap element)))))

(define (qmap-partition! predicate src-qmap t-qmap f-qmap)
  (qmap-for-each src-qmap
    (lambda (element quantity)
      (if (predicate element)
          (qmap-set! t-qmap element quantity)
          (qmap-set! f-qmap element quantity)))))

;; It is not safe to modify keys of qmap while iterating over it, so we need
;; to use an intermediate list to keep to-be-removed elements until the end
;; of the loop.

(define (qmap-filter/in-place! qmap predicate)
  (qmap-delete! qmap
    (qmap-fold qmap '()
      (lambda (element quantity removed)
        (if (predicate element)
            removed
            (cons element removed))))))

(define (qmap-remove/in-place! qmap predicate)
  (qmap-delete! qmap
    (qmap-fold qmap '()
      (lambda (element quantity removed)
        (if (predicate element)
            (cons element removed)
            removed)))))

(define (qmap-partition/in-place! predicate t-qmap f-qmap)
  (qmap-delete! t-qmap
    (qmap-fold t-qmap '()
      (lambda (element quantity removed)
        (if (predicate element)
            removed
            (begin
              (qmap-set! f-qmap element quantity)
              (cons element removed)))))))


;; Set theory operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; By John Cowan's brilliance, set theory operations are perfectly extended
;; into the domain of bags so we can use the same code everywhere. Remember
;; that sets are effectively bags which have element quantity capped at 1.

(define (qmap-union! dst-qmap src-qmap)
  (qmap-for-each src-qmap
    (lambda (element quantity)
      (qmap-set! dst-qmap element
        (max (qmap-quantity dst-qmap element) quantity)))))

(define (qmap-intersection! dst-qmap src-qmap)
  (qmap-for-each dst-qmap
    (lambda (element quantity)
      (qmap-set! dst-qmap element
        (min (qmap-quantity src-qmap element) quantity)))))

(define (qmap-sum! dst-qmap src-qmap)
  (qmap-for-each src-qmap
    (lambda (element quantity)
      (qmap-set! dst-qmap element
        (+ (qmap-quantity dst-qmap element) quantity)))))

(define (qmap-difference! dst-qmap src-qmap)
  (qmap-for-each src-qmap
    (lambda (element quantity)
      (qmap-set! dst-qmap element
        (- (qmap-quantity dst-qmap element) quantity)))))

(define (qmap-xor! dst-qmap src-qmap)
  (qmap-for-each src-qmap
    (lambda (element quantity)
      (qmap-set! dst-qmap element
        (abs (- (qmap-quantity dst-qmap element) quantity))))))


;; Subsets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qmap=? qmap-A qmap-B)
  ; They are certainly not equal if they contain different number of elements
  (if (not (= (qmap-size qmap-A) (qmap-size qmap-B))) #f
      (qmap-for-each/stop qmap-A
        (lambda (element quantity-A stop)
          (let ((quantity-B (qmap-quantity qmap-B element)))
            ; If some element quantity differs, qmaps are not equal
            (unless (= quantity-A quantity-B)
              (stop #f))))
        ; All elements of A are present in B in the same quantity,
        ; and B does not contain other elements. Qmaps are equal.
        (lambda () #t))))
;;
;; A ⊂ B ⇔ (B \ A ≠ Ø) ∧ (A \ B = Ø)
;;
(define (qmap<? qmap-A qmap-B)
  (let* ((seen-< #f)
         (all-<= (qmap-for-each/stop qmap-A
           (lambda (element quantity-A stop)
             (let ((quantity-B (qmap-quantity qmap-B element)))
               (when (> quantity-A quantity-B)
                 ; (A \ B ≠ Ø) as we have leftover elements after substraction
                 (stop #f))
               (when (< quantity-A quantity-B)
                 ; (B \ A ≠ Ø) as we have leftover elements after substraction
                 (set! seen-< #t))))
           (lambda () #t))))
    (cond
      ; if (A \ B ≠ Ø) then A ⊂ B can't be true
      ((not all-<=) #f)
      ; (A \ B = Ø) ∧ (B \ A ≠ Ø), hooray
      (seen-<       #t)
      ; now we know that (A \ B = Ø), but for other direction we are only sure
      ; in that (A ∩ B = A)--every element in A has the same quantity in B--so
      ; the only way to get (B \ A ≠ Ø) to be true is when B has more elements
      (else (< (qmap-size qmap-A) (qmap-size qmap-B))))))

;;
;; A ⊆ B ⇔ (A \ B = Ø)
;;
(define (qmap<=? qmap-A qmap-B)
  (qmap-for-each/stop qmap-A
    (lambda (element quantity-A stop)
      (let ((quantity-B (qmap-quantity qmap-B element)))
        (when (> quantity-A quantity-B)
          ; (A \ B ≠ Ø) as we have leftover elements after substraction
          (stop #f))))
    (lambda () #t)))

;; These ones can be inferred

(define (qmap>? qmap-A qmap-B)
  (qmap<? qmap-B qmap-A))

(define (qmap>=? qmap-A qmap-B)
  (qmap<=? qmap-B qmap-A))
