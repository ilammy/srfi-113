;; sets-and-bags.scm -- interface definitions of sets and bags
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-113/blob/master/LICENSE

;; Both sets and bags are implemented in terms of quantity maps (qmaps) which
;; represent mutable (element -> quantity) mappings. Obviously, sets consist
;; of (T -> {1}) mappings while bags are using (T -> {1, 2, 3, ...}) ones.
;;
;; This gives us some abstraction layer over hash tables or whatever is actually
;; used to implement qmaps. It can be used to switch implementation backend or
;; to provide optimized implementations of some primitive procedures. Refer to
;; "qmaps.scm" for these definitions.
;;
;; Obviously, primitive procedures alone are not expressive enough so a bunch
;; of set/bag-specific library procedures are also used. See "qmap-library.scm"
;; for these ones.


;; Types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type Set
  (make-set comparator qmap)
  set?
  (comparator set-element-comparator)
  (qmap       set-qmap))

(define-record-type Bag
  (make-bag comparator qmap)
  bag?
  (comparator bag-element-comparator)
  (qmap       bag-qmap))


;; Conveniences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-empty-set comparator)
  (make-set comparator (make-qmap comparator)))

(define (make-empty-bag comparator)
  (make-bag comparator (make-qmap comparator)))

(define (copy-empty-set set)
  (make-empty-set (set-element-comparator set)))

(define (copy-empty-bag bag)
  (make-empty-bag (bag-element-comparator bag)))

; Say no to forgotten return values after stateful computations
(define-syntax let/return
  (syntax-rules ()
    ((_ ((var expr) ...) body1 body2 ...)
     (let ((var expr) ...)
       body1 body2 ...
       (values var ...)))))


;; Constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set comparator . elements)
  (list->set comparator elements))

(define (bag comparator . elements)
  (list->bag comparator elements))

(define (set-unfold comparator stop? mapper successor seed)
  (let/return ((set (make-empty-set comparator)))
    (qmap-unfold! (set-qmap set) stop? mapper successor seed qmap-add/set!)))

(define (bag-unfold comparator stop? mapper successor seed)
  (let/return ((bag (make-empty-bag comparator)))
    (qmap-unfold! (bag-qmap bag) stop? mapper successor seed qmap-add/bag!)))


;; Predicates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-contains? set element)
  (qmap-contains? (set-qmap set) element))

(define (bag-contains? bag element)
  (qmap-contains? (bag-qmap bag) element))

(define (set-empty? set)
  (qmap-empty? (set-qmap set)))

(define (bag-empty? bag)
  (qmap-empty? (bag-qmap bag)))

(define (set-disjoint? set1 set2)
  (qmap-disjoint? (set-qmap set1) (set-qmap set2)))

(define (bag-disjoint? bag1 bag2)
  (qmap-disjoint? (bag-qmap bag1) (bag-qmap bag2)))


;; Accessors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-member set element default)
  (qmap-member (set-qmap set) element default))

(define (bag-member bag element default)
  (qmap-member (bag-qmap bag) element default))

(define (bag-element-count bag element)
  (qmap-quantity (bag-qmap bag) element))


;; Updaters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; adjoin

(define (set-adjoin set . elements)
  (let/return ((set (set-copy set)))
    (qmap-adjoin! (set-qmap set) elements qmap-add/set!)))

(define (bag-adjoin bag . elements)
  (let/return ((bag (bag-copy bag)))
    (qmap-adjoin! (bag-qmap bag) elements qmap-add/bag!)))

(define (set-adjoin! set . elements)
  (let/return ((set set))
    (qmap-adjoin! (set-qmap set) elements qmap-add/set!)))

(define (bag-adjoin! bag . elements)
  (let/return ((bag bag))
    (qmap-adjoin! (bag-qmap bag) elements qmap-add/bag!)))

; replace

(define (set-replace set element)
  (let/return ((set (set-copy set)))
    (qmap-replace! (set-qmap set) element)))

(define (bag-replace bag element)
  (let/return ((bag (bag-copy bag)))
    (qmap-replace! (bag-qmap bag) element)))

(define (set-replace! set element)
  (let/return ((set set))
    (qmap-replace! (set-qmap set) element)))

(define (bag-replace! bag element)
  (let/return ((bag bag))
    (qmap-replace! (bag-qmap bag) element)))

; delete

(define (set-delete set element . others)
  (let/return ((set (set-copy set)))
    (qmap-rem! (set-qmap set) element)
    (qmap-delete! (set-qmap set) others)))

(define (bag-delete bag element . others)
  (let/return ((bag (bag-copy bag)))
    (qmap-rem! (bag-qmap bag) element)
    (qmap-delete! (bag-qmap bag) others)))

(define (set-delete! set element . others)
  (let/return ((set set))
    (qmap-rem! (set-qmap set) element)
    (qmap-delete! (set-qmap set) others)))

(define (bag-delete! bag element . others)
  (let/return ((bag bag))
    (qmap-rem! (bag-qmap bag) element)
    (qmap-delete! (bag-qmap bag) others)))

; delete-all

(define (set-delete-all set elements)
  (let/return ((set (set-copy set)))
    (qmap-delete! (set-qmap set) elements)))

(define (bag-delete-all bag elements)
  (let/return ((bag (bag-copy bag)))
    (qmap-delete! (bag-qmap bag) elements)))

(define (set-delete-all! set elements)
  (let/return ((set set))
    (qmap-delete! (set-qmap set) elements)))

(define (bag-delete-all! bag elements)
  (let/return ((bag bag))
    (qmap-delete! (bag-qmap bag) elements)))

; search

(define (set-search! set element failure success)
  (values set
          (qmap-search! (set-qmap set) element failure success)))

(define (bag-search! bag element failure success)
  (values bag
          (qmap-search! (bag-qmap bag) element failure success)))

; bag-specific

(define (bag-increment! bag element count)
  (let/return ((bag bag))
    (qmap-inc! (bag-qmap bag) element count)))

(define (bag-decrement! bag element count)
  (let/return ((bag bag))
    (qmap-dec! (bag-qmap bag) element count)))


;; The whole set/bag ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-size set)
  (qmap-size (set-qmap set)))

(define (bag-unique-size bag)
  (qmap-size (bag-qmap bag)))

(define (bag-size bag)
  (qmap-count (bag-qmap bag) (lambda (element) #t)))

(define (set-find predicate set failure)
  (qmap-find (set-qmap set) predicate failure))

(define (bag-find predicate bag failure)
  (qmap-find (bag-qmap bag) predicate failure))

(define (set-count predicate set)
  (qmap-count (set-qmap set) predicate))

(define (bag-count predicate bag)
  (qmap-count (bag-qmap bag) predicate))

(define (set-any? predicate set)
  (qmap-any? (set-qmap set) predicate))

(define (bag-any? predicate bag)
  (qmap-any? (bag-qmap bag) predicate))

(define (set-every? predicate set)
  (qmap-every? (set-qmap set) predicate))

(define (bag-every? predicate bag)
  (qmap-every? (bag-qmap bag) predicate))


;; Mapping and folding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-map comparator proc src-set)
  (let/return ((dst-set (make-empty-set comparator)))
    (qmap-map! (set-qmap dst-set) (set-qmap src-set) proc qmap-add/set!)))

(define (bag-map comparator proc src-bag)
  (let/return ((dst-bag (make-empty-bag comparator)))
    (qmap-map! (bag-qmap dst-bag) (bag-qmap src-bag) proc qmap-add/bag!)))

(define (set-for-each proc set)
  (qmap-for-each/set (set-qmap set) proc))

(define (bag-for-each proc bag)
  (qmap-for-each/bag (bag-qmap bag) proc))

(define (bag-for-each-unique proc bag)
  (qmap-for-each (bag-qmap bag) proc))

(define (set-fold kons knil set)
  (qmap-fold/set (set-qmap set) knil kons))

(define (bag-fold kons knil bag)
  (qmap-fold/bag (bag-qmap bag) knil kons))

(define (bag-fold-unique kons knil bag)
  (qmap-fold (bag-qmap bag) knil kons))

; filter

(define (set-filter predicate src-set)
  (let/return ((dst-set (copy-empty-set src-set)))
    (qmap-filter! (set-qmap dst-set) predicate (set-qmap src-set))))

(define (bag-filter predicate src-bag)
  (let/return ((dst-bag (copy-empty-bag src-bag)))
    (qmap-filter! (bag-qmap dst-bag) predicate (bag-qmap src-bag))))

(define (set-filter! predicate set)
  (let/return ((set set))
    (qmap-filter/in-place! (set-qmap set) predicate)))

(define (bag-filter! predicate bag)
  (let/return ((bag bag))
    (qmap-filter/in-place! (bag-qmap bag) predicate)))

; remove

(define (set-remove predicate src-set)
  (let/return ((dst-set (set-copy src-set)))
    (qmap-remove! (set-qmap dst-set) predicate (set-qmap src-set))))

(define (bag-remove predicate src-bag)
  (let/return ((dst-bag (bag-copy src-bag)))
    (qmap-remove! (bag-qmap dst-bag) predicate (bag-qmap src-bag))))

(define (set-remove! predicate set)
  (let/return ((set set))
    (qmap-remove/in-place! (set-qmap set) predicate)))

(define (bag-remove! predicate bag)
  (let/return ((bag bag))
    (qmap-remove/in-place! (bag-qmap bag) predicate)))

; partition

(define (set-partition predicate src-set)
  (let/return ((t-set (copy-empty-set src-set))
               (f-set (copy-empty-set src-set)))
    (qmap-partition! predicate
      (set-qmap src-set) (set-qmap t-set) (set-qmap f-set))))

(define (bag-partition predicate src-bag)
  (let/return ((t-bag (copy-empty-bag src-bag))
               (f-bag (copy-empty-bag src-bag)))
    (qmap-partition! predicate
      (bag-qmap src-bag) (bag-qmap t-bag) (bag-qmap f-bag))))

(define (set-partition! predicate t-set)
  (let/return ((t-set t-set)
               (f-set (copy-empty-set t-set)))
    (qmap-partition/in-place! predicate
      (set-qmap t-set) (set-qmap f-set))))

(define (bag-partition! predicate t-bag)
  (let/return ((t-bag t-bag)
               (f-bag (copy-empty-bag t-bag)))
    (qmap-partition/in-place! predicate
      (bag-qmap t-bag) (bag-qmap f-bag))))

; bag-specific

(define (bag-sum bag . others)
  (let/return ((bag (bag-copy bag)))
    (for-each
      (lambda (other-bag) (qmap-sum! (bag-qmap bag) (bag-qmap other-bag)))
      others)))

(define (bag-sum! bag . others)
  (let/return ((bag bag))
    (for-each
      (lambda (other-bag) (qmap-sum! (bag-qmap bag) (bag-qmap other-bag)))
      others)))

(define (bag-product n bag)
  (let/return ((bag (bag-copy bag)))
    (qmap-product! (bag-qmap bag) n)))

(define (bag-product! n bag)
  (let/return ((bag bag))
    (qmap-product! (bag-qmap bag) n)))


;; Copying and conversion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-copy set)
  (make-set (set-element-comparator set)
            (qmap-copy (set-qmap set))))

(define (bag-copy bag)
  (make-bag (bag-element-comparator bag)
            (qmap-copy (bag-qmap bag))))

(define (set->list set)
  (set-fold cons '() set))

(define (bag->list bag)
  (bag-fold cons '() bag))

(define (list->set comparator elements)
  (list->set! (make-empty-set comparator) elements))

(define (list->bag comparator elements)
  (list->bag! (make-empty-bag comparator) elements))

(define (list->set! set list)
  (let/return ((set set))
    (qmap-adjoin! (set-qmap set) list qmap-add/set!)))

(define (list->bag! bag list)
  (let/return ((bag bag))
    (qmap-adjoin! (bag-qmap bag) list qmap-add/bag!)))

(define (bag->alist bag)
  (qmap-fold (bag-qmap bag) '()
    (lambda (element quantity pairs)
      (cons (cons element quantity) pairs))))

(define (alist->bag comparator alist)
  (let/return ((bag (make-empty-bag comparator)))
    (let ((qmap (bag-qmap bag)))
      (for-each
        (lambda (pair) (qmap-add/bag! qmap (car pair) (cdr pair)))
        alist))))

(define (bag->set bag)
  (let/return ((set (make-empty-set (bag-element-comparator bag))))
    (let ((set-qmap (set-qmap set)))
      (qmap-for-each (bag-qmap bag)
        (lambda (element quantity)
          (qmap-add/set! set-qmap element 1))))))

(define (set->bag set)
  (make-bag (set-element-comparator set)
            (qmap-copy (set-qmap set))))

(define (set->bag! bag set)
  (let/return ((bag bag))
    (let ((bag-qmap (bag-qmap bag)))
      (qmap-for-each (set-qmap set)
        (lambda (element quantity)
          (qmap-add/bag! bag-qmap element 1))))))


;; Subsets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In hope that the implementation can optimize case-lambdas
(define-syntax subset-predicate
  (syntax-rules ()
    ((_ foo-qmap qmap-pred?)
     (case-lambda
       ((foo) #t)
       ((foo1 foo2) (qmap-pred? (foo-qmap foo1) (foo-qmap foo2)))
       ((foo . others)
        (pairwise-and qmap-pred? foo-qmap (cons foo others)))))))

(define (pairwise-and pred mapper list)
  (let loop ((list list))
    (if (null? (cdr list)) #t
        (if (pred (mapper (car list)) (mapper (cadr list)))
            (loop (cdr list))
            #f))))

(define set=?
  (subset-predicate set-qmap qmap=?))

(define set<?
  (subset-predicate set-qmap qmap<?))

(define set>?
  (subset-predicate set-qmap qmap>?))

(define set<=?
  (subset-predicate set-qmap qmap<=?))

(define set>=?
  (subset-predicate set-qmap qmap>=?))

(define bag=?
  (subset-predicate bag-qmap qmap=?))

(define bag<?
  (subset-predicate bag-qmap qmap<?))

(define bag>?
  (subset-predicate bag-qmap qmap>?))

(define bag<=?
  (subset-predicate bag-qmap qmap<=?))

(define bag>=?
  (subset-predicate bag-qmap qmap>=?))


;; Set theory operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; union

(define (set-union set . others)
  (let/return ((set (set-copy set)))
    (for-each
      (lambda (other-set) (qmap-union! (set-qmap set) (set-qmap other-set)))
      others)))

(define (bag-union bag . others)
  (let/return ((bag (bag-copy bag)))
    (for-each
      (lambda (other-bag) (qmap-union! (bag-qmap bag) (bag-qmap other-bag)))
      others)))

(define (set-union! set . others)
  (let/return ((set set))
    (for-each
      (lambda (other-set) (qmap-union! (set-qmap set) (set-qmap other-set)))
      others)))

(define (bag-union! bag . others)
  (let/return ((bag bag))
    (for-each
      (lambda (other-bag) (qmap-union! (bag-qmap bag) (bag-qmap other-bag)))
      others)))

; intersection

(define (set-intersection set . others)
  (let/return ((set (set-copy set)))
    (for-each
      (lambda (other-set) (qmap-intersection! (set-qmap set) (set-qmap other-set)))
      others)))

(define (bag-intersection bag . others)
  (let/return ((bag (bag-copy bag)))
    (for-each
      (lambda (other-bag) (qmap-intersection! (bag-qmap bag) (bag-qmap other-bag)))
      others)))

(define (set-intersection! set . others)
  (let/return ((set set))
    (for-each
      (lambda (other-set) (qmap-intersection! (set-qmap set) (set-qmap other-set)))
      others)))

(define (bag-intersection! bag . others)
  (let/return ((bag bag))
    (for-each
      (lambda (other-bag) (qmap-intersection! (bag-qmap bag) (bag-qmap other-bag)))
      others)))

; asymmetric difference

(define (set-difference set . others)
  (let/return ((set (set-copy set)))
    (for-each
      (lambda (other-set) (qmap-difference! (set-qmap set) (set-qmap other-set)))
      others)))

(define (bag-difference bag . others)
  (let/return ((bag (bag-copy bag)))
    (for-each
      (lambda (other-bag) (qmap-difference! (bag-qmap bag) (bag-qmap other-bag)))
      others)))

(define (set-difference! set . others)
  (let/return ((set set))
    (for-each
      (lambda (other-set) (qmap-difference! (set-qmap set) (set-qmap other-set)))
      others)))

(define (bag-difference! bag . others)
  (let/return ((bag bag))
    (for-each
      (lambda (other-bag) (qmap-difference! (bag-qmap bag) (bag-qmap other-bag)))
      others)))

; symmetric difference

(define (set-xor set1 set2)
  (let/return ((set3 (set-copy set1)))
    (qmap-xor! (set-qmap set3) (set-qmap set2))))

(define (bag-xor bag1 bag2)
  (let/return ((bag3 (bag-copy bag1)))
    (qmap-xor! (bag-qmap bag3) (bag-qmap bag2))))

(define (set-xor! set1 set2)
  (let/return ((set1 set1))
    (qmap-xor! (set-qmap set1) (set-qmap set2))))

(define (bag-xor! bag1 bag2)
  (let/return ((bag1 bag1))
    (qmap-xor! (bag-qmap bag1) (bag-qmap bag2))))


;; Comparators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set<=? or bag<=? do not define a partial order on sets and bags
;; so comparators do not define a comparison procedure.

(define set-comparator
  (make-comparator #t set=? #f hash))

(define bag-comparator
  (make-comparator #t bag=? #f hash))
