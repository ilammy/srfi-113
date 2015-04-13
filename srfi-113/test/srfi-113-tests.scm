(import (chibi) (chibi test) (srfi 1) (srfi 113) (srfi 114))

(test-begin "srfi-113")

(test-begin "sets")

(test-group "Constructors & predicates"
  (define empty-set (set integer-comparator))
  (test #t (set? empty-set))
  (test #t (set-empty? empty-set))

  (define full-set (set integer-comparator 1 2 3 2 1))
  (test #t (set? full-set))
  (test #f (set-empty? full-set))
  (test #t (set-contains? full-set 1))
  (test #t (set-contains? full-set 2))
  (test #t (set-contains? full-set 3))
  (test #f (set-contains? full-set 4))

  (define test-set1
    (set-unfold integer-comparator
      (lambda (n) (< n 0))
      (lambda (n) (* 2 n))
      (lambda (n) (- n 1))
      4))
  (test #t (set? test-set1))
  (test #f (set-empty? test-set1))
  (test #f (set-contains? test-set1 10))
  (test #t (set-contains? test-set1 8))
  (test #t (set-contains? test-set1 6))
  (test #t (set-contains? test-set1 4))
  (test #f (set-contains? test-set1 3))
  (test #t (set-contains? test-set1 2))
  (test #f (set-contains? test-set1 1))
  (test #t (set-contains? test-set1 0))
  (test #f (set-contains? test-set1 -2))

  (define test-set2
    (set-unfold integer-comparator
      (lambda (n) #t)
      (lambda (n) n)
      (lambda (n) (+ n 1))
      4))
  (test #t (set? test-set2))
  (test #t (set-empty? test-set2))

  (define test-set3
    (set-unfold integer-comparator
      (lambda (n) (< n 0))
      (lambda (n) 42)
      (lambda (n) (- n 1))
      3))
  (test #t (set? test-set3))
  (test #f (set-empty? test-set3))
  (test #f (set-contains? test-set3 0))
  (test #f (set-contains? test-set3 1))
  (test #f (set-contains? test-set3 2))
  (test #f (set-contains? test-set3 3))
  (test #t (set-contains? test-set3 42))

  (define set1 (set integer-comparator 1 2 3))
  (define set2 (set integer-comparator 3 4 5))
  (define set3 (set integer-comparator 5 6 7))
  (test #f (set-disjoint? set1 set2))
  (test #f (set-disjoint? set2 set3))
  (test #t (set-disjoint? set3 set1))
  (test #t (set-disjoint? set1 empty-set))
  (test #t (set-disjoint? set2 empty-set))
  (test #t (set-disjoint? set3 empty-set))
  (test #f (set-disjoint? set1 set1))
  (test #f (set-disjoint? set2 set2))
  (test #f (set-disjoint? set3 set3))
  (test #t (set-disjoint? empty-set empty-set)))

(test-group "Accessors & the whole set"
  (define strange-comparator
    (make-comparator integer?
      (lambda (a b) (if (and (even? a) (even? b)) #t (= a b)))
      #f
      (let ((hash (comparator-hash-function integer-comparator)))
        (lambda (n) (if (even? n) 0 (hash n))))))

  (define empty-set (set integer-comparator))
  (define full-set (set number-comparator 1 2 3))
  (define test-set (set strange-comparator 0 1 2 3 4 5))

  (test integer-comparator (set-element-comparator empty-set))
  (test '() (set-member empty-set 42 '()))
  (test '() (set-member empty-set 0 '()))

  (test number-comparator (set-element-comparator full-set))
  (test  1 (set-member full-set 1 #f))
  (test  2 (set-member full-set 2 #f))
  (test  3 (set-member full-set 3 #f))
  (test #f (set-member full-set 4 #f))

  (test strange-comparator (set-element-comparator test-set))
  (test #t (= (set-member test-set 0 #t)
              (set-member test-set 2 #t)
              (set-member test-set 4 #t)
              (set-member test-set 6 #t)))
  (test  1 (set-member test-set 1 #t))
  (test  3 (set-member test-set 3 #t))
  (test  5 (set-member test-set 5 #t))
  (test #t (set-member test-set 7 #t))

  (test 0 (set-size empty-set))
  (test 3 (set-size full-set))
  (test 4 (set-size test-set))

  (test '() (set-find even? empty-set (lambda () '())))
  (test #t (even? (set-find even? full-set (lambda () '()))))
  (test #t (odd?  (set-find odd?  test-set (lambda () 0))))

  (test 0 (set-count even? empty-set))
  (test 1 (set-count even? full-set))
  (test 1 (set-count even? test-set))

  (test 0 (set-count odd? empty-set))
  (test 2 (set-count odd? full-set))
  (test 3 (set-count odd? test-set))

  (test #f (set-any? even? empty-set))
  (test #t (set-any? even? full-set))
  (test #t (set-any? even? test-set))
  (test #f (set-any? (lambda (n) (> n 5)) empty-set))
  (test #f (set-any? (lambda (n) (> n 5)) full-set))
  (test #f (set-any? (lambda (n) (> n 5)) test-set))

  (test #t (set-every? even? empty-set))
  (test #f (set-every? even? full-set))
  (test #f (set-every? even? test-set))
  (test #t (set-every? (lambda (n) (< n 10)) empty-set))
  (test #t (set-every? (lambda (n) (< n 10)) full-set))
  (test #t (set-every? (lambda (n) (< n 10)) test-set)))

(test-group "Subsets"
  (define empty-set (set integer-comparator))
  (define set1 (set integer-comparator 1 2 3))
  (define set2 (set integer-comparator 1 2 3 4 5))
  (define set3 (set integer-comparator 4))
  (define set4 (set integer-comparator 4 5 6))

  (test #t (set=? empty-set empty-set))
  (test #t (set=? set1 set1))
  (test #t (set=? set1 (set integer-comparator 3 2 1)))
  (test #f (set=? set1 set2))
  (test #t (set=? (set integer-comparator 1) (set integer-comparator 1) (set integer-comparator 1)))
  (test #f (set=? set2 set4))

  (test #t (set<? empty-set set1 set2))
  (test #f (set<? set3 set4 set2))
  (test #f (set<? set3 set1))
  (test #t (set<? set3 set2))
  (test #t (set<? set3 set4))
  (test #f (set<? set2 set4))
  (test #f (set<? set4 set2))
  (test #f (set<? set4 empty-set))
  (test #f (set<? empty-set empty-set))
  (test #f (set<? set1 set1))

  (test #t (set>? set2 set1 empty-set))
  (test #f (set>? set2 set4 set3))
  (test #f (set>? set3 set1))
  (test #t (set>? set2 set3))
  (test #t (set>? set4 set3))
  (test #f (set>? set4 set2))
  (test #f (set>? set2 set4))
  (test #f (set>? empty-set set4))
  (test #f (set>? empty-set empty-set))
  (test #f (set>? set1 set1))

  (test #t (set<=? empty-set set1 set2))
  (test #f (set<=? set3 set4 set2))
  (test #f (set<=? set3 set1))
  (test #t (set<=? set3 set2))
  (test #t (set<=? set3 set4))
  (test #f (set<=? set2 set4))
  (test #f (set<=? set4 set2))
  (test #f (set<=? set4 empty-set))
  (test #t (set<=? empty-set empty-set))
  (test #t (set<=? set1 set1))

  (test #t (set>=? set2 set1 empty-set))
  (test #f (set>=? set2 set4 set3))
  (test #f (set>=? set3 set1))
  (test #t (set>=? set2 set3))
  (test #t (set>=? set4 set3))
  (test #f (set>=? set4 set2))
  (test #f (set>=? set2 set4))
  (test #f (set>=? empty-set set4))
  (test #t (set>=? empty-set empty-set))
  (test #t (set>=? set1 set1)))

(test-group "Copying and conversion"
  (define set1 (set integer-comparator 0 1 9))
  (define set2 (set-copy set1))
  (test #t (set? set2))
  (test integer-comparator (set-element-comparator set2))
  (test #t (set=? set1 set2))

  (test #t (lset= equal? '()
    (set->list (set integer-comparator))))
  (test #t (lset= equal? '(1 2 3)
    (set->list (set integer-comparator 1 2 3))))
  (test #t (lset= equal? '(1)
    (set->list (set integer-comparator 1 1 1 1 1 1))))

  (define set3 (list->set integer-comparator '()))
  (test #t (set? set3))
  (test #t (set-empty? set3))
  (test integer-comparator (set-element-comparator set3))

  (define set4 (list->set integer-comparator '(1 1 1)))
  (test #t (set? set4))
  (test 1 (set-size set4))
  (test #t (set-contains? set4 1))
  (test integer-comparator (set-element-comparator set4))

  (define set5 (list->set integer-comparator '(1 4 6)))
  (test #t (set? set5))
  (test 3 (set-size set5))
  (test #t (set-contains? set5 1))
  (test #t (set-contains? set5 4))
  (test #t (set-contains? set5 6))
  (test integer-comparator (set-element-comparator set5))

  (define set6 (set integer-comparator 1 2 3))
  (test #f (set-contains? set6 4))

  (define set7 (list->set! (set-copy set6) '(4 4 5)))
  (test #t (set-contains? set7 4))

  (define set8 (list->set! (set-copy set7) '()))
  (test #t (set=? set7 set8))

  (test #t (lset= equal? '(1 2 3 4 5) (set->list set8))))

(test-group "Mapping and folding"
  (test #t (set=? (set integer-comparator)
    (set-map integer-comparator
      (lambda (x) x)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator 2 4 6)
    (set-map integer-comparator
      (lambda (x) (* 2 x))
      (set integer-comparator 1 2 3))))

  (let ((even-count 0) (invoke-count 0))
    (set-for-each
      (lambda (x)
        (set! invoke-count (+ 1 invoke-count))
        (if (even? x)
            (set! even-count (+ 1 even-count))
            #f))
      (set integer-comparator 1 2 3 4 5 2))
    (test 2 even-count)
    (test 5 invoke-count))
  (let ((invoke-count 0))
    (set-for-each
      (lambda (x) (set! invoke-count (+ 1 invoke-count)))
      (set integer-comparator))
    (test 0 invoke-count))

  (test 64 (set-fold + 0 (set integer-comparator 5 3 56)))
  (test #f (set-fold + #f (set integer-comparator)))

  (test #t (set=? (set integer-comparator 2 4 6)
    (set-filter even? (set integer-comparator 1 2 3 4 5 6 7))))
  (test #t (set=? (set integer-comparator)
    (set-filter even? (set integer-comparator 1 5 7))))
  (test #t (set=? (set integer-comparator)
    (set-filter (lambda (x) #t) (set integer-comparator))))

  (test #t (set=? (set integer-comparator 2 4 6)
    (set-filter! even? (set integer-comparator 1 2 3 4 5 6 7))))
  (test #t (set=? (set integer-comparator)
    (set-filter! even? (set integer-comparator 1 5 7))))
  (test #t (set=? (set integer-comparator)
    (set-filter! (lambda (x) #t) (set integer-comparator))))

  (test #t (set=? (set integer-comparator 5 6)
    (set-remove (lambda (x) (< x 5)) (set integer-comparator 1 2 3 4 5 6))))
  (test #t (set=? (set integer-comparator)
    (set-remove (lambda (x) (< x 5)) (set integer-comparator -1 0 +1))))
  (test #t (set=? (set integer-comparator 8 9 10)
    (set-remove (lambda (x) (< x 5)) (set integer-comparator 8 9 10))))

  (test #t (set=? (set integer-comparator 5 6)
    (set-remove! (lambda (x) (< x 5)) (set integer-comparator 1 2 3 4 5 6))))
  (test #t (set=? (set integer-comparator)
    (set-remove! (lambda (x) (< x 5)) (set integer-comparator -1 0 +1))))
  (test #t (set=? (set integer-comparator 8 9 10)
    (set-remove! (lambda (x) (< x 5)) (set integer-comparator 8 9 10))))

  (call-with-values
    (lambda () (set-partition number? (set integer-comparator)))
    (lambda (nope1 nope2)
      (test #t (set-empty? nope1))
      (test #t (set-empty? nope2))))
  (call-with-values
    (lambda () (set-partition even? (set integer-comparator 1 2 3 4 5)))
    (lambda (even odd)
      (test #t (set=? even (set integer-comparator 2 4)))
      (test #t (set=? odd (set integer-comparator 1 3 5)))))
  (call-with-values
    (lambda () (set-partition (lambda (n) (< n 10)) (set integer-comparator 1 2 3 4 5)))
    (lambda (small big)
      (test #t (set=? small (set integer-comparator 1 2 3 4 5)))
      (test #t (set-empty? big))))

  (call-with-values
    (lambda () (set-partition! number? (set integer-comparator)))
    (lambda (nope1 nope2)
      (test #t (set-empty? nope1))
      (test #t (set-empty? nope2))))
  (call-with-values
    (lambda () (set-partition! even? (set integer-comparator 1 2 3 4 5)))
    (lambda (even odd)
      (test #t (set=? even (set integer-comparator 2 4)))
      (test #t (set=? odd (set integer-comparator 1 3 5)))))
  (call-with-values
    (lambda () (set-partition! (lambda (n) (< n 10)) (set integer-comparator 1 2 3 4 5)))
    (lambda (small big)
      (test #t (set=? small (set integer-comparator 1 2 3 4 5)))
      (test #t (set-empty? big)))))

(test-group "Updaters"
  (define set1 (set integer-comparator 1 2 3))
  (define set2 (set-adjoin set1 4 5))
  (test #t (set? set2))
  (test #t (set=? set2 (set integer-comparator 1 2 3 4 5)))

  (define set3 (set-adjoin! set1 1 2 6))
  (test #t (set=? set3 (set integer-comparator 1 2 3 6)))

  (define strange-comparator
    (make-comparator integer?
      (lambda (a b) (if (and (even? a) (even? b)) #t (= a b)))
      #f
      (let ((hash (comparator-hash-function integer-comparator)))
        (lambda (n) (if (even? n) 0 (hash n))))))

  (define set4 (set strange-comparator 0 1 2 3 4))
  (test #t (set=? set4 (set strange-comparator 0 1 3)))
  (test #t (set=? set4 (set strange-comparator 1 2 3)))
  (test #t (set=? set4 (set strange-comparator 1 3 4)))
  (test 0 (set-member set4 0 #f))
  (test 0 (set-member set4 2 #f))
  (test 0 (set-member set4 4 #f))

  (define set5 (set-replace (set strange-comparator 0 1 2 3 4) 4))
  (test #t (set=? set5 (set strange-comparator 0 1 3)))
  (test #t (set=? set5 (set strange-comparator 1 2 3)))
  (test #t (set=? set5 (set strange-comparator 1 3 4)))
  (test 4 (set-member set5 0 #f))
  (test 4 (set-member set5 2 #f))
  (test 4 (set-member set5 4 #f))

  (define set6 (set-replace! (set strange-comparator 0 1 2 3 4) 2))
  (test #t (set=? set6 (set strange-comparator 0 1 3)))
  (test #t (set=? set6 (set strange-comparator 1 2 3)))
  (test #t (set=? set6 (set strange-comparator 1 3 4)))
  (test 2 (set-member set6 0 #f))
  (test 2 (set-member set6 2 #f))
  (test 2 (set-member set6 4 #f))

  (define set7 (set integer-comparator 1 5 8 9))

  (test #t (set=? (set integer-comparator 1 5)
    (set-delete set7 8 9)))
  (test #t (set=? (set integer-comparator 1 5 9)
    (set-delete set7 8 10 11 47)))
  (test #t (set-empty? (set-delete set7 1 5 8 9 5 9 1)))

  (test #t (set=? (set integer-comparator 1 5)
    (set-delete! (set-copy set7) 8 9)))
  (test #t (set=? (set integer-comparator 1 5 9)
    (set-delete! (set-copy set7) 8 10 11 47)))
  (test #t (set-empty? (set-delete! (set-copy set7) 1 5 8 9 5 9 1)))

  (test #t (set=? (set integer-comparator 1 5)
    (set-delete-all set7 '(8 9))))
  (test #t (set=? (set integer-comparator 1 5 9)
    (set-delete-all set7 '(8 10 11 47))))
  (test #t (set-empty? (set-delete-all set7 '(1 5 8 9 5 9 1))))

  (test #t (set=? (set integer-comparator 1 5)
    (set-delete-all! (set-copy set7) '(8 9))))
  (test #t (set=? (set integer-comparator 1 5 9)
    (set-delete-all! (set-copy set7) '(8 10 11 47))))
  (test #t (set-empty? (set-delete-all! (set-copy set7) '(1 5 8 9 5 9 1))))

  (define set8 (set integer-comparator 5 8 1))

  (call-with-values
    (lambda ()
      (set-search! (set-copy set8) 0
        (lambda (insert ignore)
          (insert 'insert))
        (lambda (element update remove) #f)))
    (lambda (set8 obj)
      (test 'insert obj)
      (test #t (set=? set8 (set integer-comparator 5 8 1 0)))))

  (call-with-values
    (lambda ()
      (set-search! (set-copy set8) 0
        (lambda (insert ignore) (ignore 'ignore))
        (lambda (element update remove) #f)))
    (lambda (set8 obj)
      (test 'ignore obj)
      (test #t (set=? set8 (set integer-comparator 5 8 1)))))

  (call-with-values
    (lambda ()
      (set-search! (set-copy set8) 5
        (lambda (insert ignore) #f)
        (lambda (element update remove)
          (test 5 element)
          (update 9 'update))))
    (lambda (set8 obj)
      (test 'update obj)
      (test #t (set=? set8 (set integer-comparator 9 8 1)))))

  (call-with-values
    (lambda ()
      (set-search! (set-copy set8) 5
        (lambda (insert ignore) #f)
        (lambda (element update remove)
          (test 5 element)
          (remove 'remove))))
    (lambda (set8 obj)
      (test 'remove obj)
      (test #t (set=? set8 (set integer-comparator 8 1))))))

(test-group "Set theory operations"
  (test #t (set=? (set integer-comparator 1 2 3 4 5 6 7)
    (set-union
      (set integer-comparator 1 2)
      (set integer-comparator 2 3)
      (set integer-comparator 4)
      (set integer-comparator 6 5 7)
      (set integer-comparator 6))))
  (test #t (set=? (set integer-comparator 5)
    (set-union
      (set integer-comparator 5)
      (set integer-comparator 5 5 5)
      (set integer-comparator)
      (set integer-comparator 5))))
  (test #t (set=? (set integer-comparator)
    (set-union
      (set integer-comparator)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator 3 2 1)
    (set-union
      (set integer-comparator 1 2 3))))

  (test #t (set=? (set integer-comparator 1 2 3 4 5 6 7)
    (set-union!
      (set integer-comparator 1 2)
      (set integer-comparator 2 3)
      (set integer-comparator 4)
      (set integer-comparator 6 5 7)
      (set integer-comparator 6))))
  (test #t (set=? (set integer-comparator 5)
    (set-union!
      (set integer-comparator 5)
      (set integer-comparator 5 5 5)
      (set integer-comparator)
      (set integer-comparator 5))))
  (test #t (set=? (set integer-comparator)
    (set-union!
      (set integer-comparator)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator 3 2 1)
    (set-union!
      (set integer-comparator 1 2 3))))

  (test #t (set=? (set integer-comparator)
    (set-intersection
      (set integer-comparator)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator)
    (set-intersection
      (set integer-comparator 1 2 3)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator)
    (set-intersection
      (set integer-comparator 1 2)
      (set integer-comparator 3 4))))
  (test #t (set=? (set integer-comparator 4)
    (set-intersection
      (set integer-comparator 1 2 3 4)
      (set integer-comparator 3 4)
      (set integer-comparator 4 5 6))))
  (test #t (set=? (set integer-comparator 2 1 3)
    (set-intersection
      (set integer-comparator 1 2 3)
      (set integer-comparator 3 2 1))))
  (test #t (set=? (set integer-comparator 3 2 1)
    (set-intersection
      (set integer-comparator 1 2 3))))

  (test #t (set=? (set integer-comparator)
    (set-intersection!
      (set integer-comparator)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator)
    (set-intersection!
      (set integer-comparator 1 2 3)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator)
    (set-intersection!
      (set integer-comparator 1 2)
      (set integer-comparator 3 4))))
  (test #t (set=? (set integer-comparator 4)
    (set-intersection!
      (set integer-comparator 1 2 3 4)
      (set integer-comparator 3 4)
      (set integer-comparator 4 5 6))))
  (test #t (set=? (set integer-comparator 2 1 3)
    (set-intersection!
      (set integer-comparator 1 2 3)
      (set integer-comparator 3 2 1))))
  (test #t (set=? (set integer-comparator 3 2 1)
    (set-intersection!
      (set integer-comparator 1 2 3))))

  (test #t (set=? (set integer-comparator 2)
    (set-difference
      (set integer-comparator 1 2 3 4 5)
      (set integer-comparator 3 4)
      (set integer-comparator 5)
      (set integer-comparator 1))))
  (test #t (set=? (set integer-comparator 1 2 3 4 5)
    (set-difference
      (set integer-comparator 1 2 3 4 5))))
  (test #t (set=? (set integer-comparator)
    (set-difference
      (set integer-comparator 1 2 3 4 5)
      (set integer-comparator 3 4 5)
      (set integer-comparator 1 2 3))))
  (test #t (set=? (set integer-comparator 2)
    (set-difference
      (set integer-comparator 1 2 3 4 5)
      (set integer-comparator 3 4 5 6 7 8 9)
      (set integer-comparator 1))))
  (test #t (set=? (set integer-comparator)
    (set-difference
      (set integer-comparator)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator)
    (set-difference
      (set integer-comparator)
      (set integer-comparator 4))))

  (test #t (set=? (set integer-comparator 2)
    (set-difference!
      (set integer-comparator 1 2 3 4 5)
      (set integer-comparator 3 4)
      (set integer-comparator 5)
      (set integer-comparator 1))))
  (test #t (set=? (set integer-comparator 1 2 3 4 5)
    (set-difference!
      (set integer-comparator 1 2 3 4 5))))
  (test #t (set=? (set integer-comparator)
    (set-difference!
      (set integer-comparator 1 2 3 4 5)
      (set integer-comparator 3 4 5)
      (set integer-comparator 1 2 3))))
  (test #t (set=? (set integer-comparator 2)
    (set-difference!
      (set integer-comparator 1 2 3 4 5)
      (set integer-comparator 3 4 5 6 7 8 9)
      (set integer-comparator 1))))
  (test #t (set=? (set integer-comparator)
    (set-difference!
      (set integer-comparator)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator)
    (set-difference!
      (set integer-comparator)
      (set integer-comparator 4))))

  (test #t (set=? (set integer-comparator 1 2 3 7 8)
    (set-xor
      (set integer-comparator 1 2 3 4 5 6)
      (set integer-comparator 4 5 6 7 8))))
  (test #t (set=? (set integer-comparator 1)
    (set-xor
      (set integer-comparator 1 2 3)
      (set integer-comparator 2 3))))
  (test #t (set=? (set integer-comparator 4)
    (set-xor
      (set integer-comparator 1 2 3)
      (set integer-comparator 1 2 3 4))))
  (test #t (set=? (set integer-comparator)
    (set-xor
      (set integer-comparator 1)
      (set integer-comparator 1))))
  (test #t (set=? (set integer-comparator 1 2 3)
    (set-xor
      (set integer-comparator)
      (set integer-comparator 1 2 3))))
  (test #t (set=? (set integer-comparator 4 5 6)
    (set-xor
      (set integer-comparator 4 5 6)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator)
    (set-xor
      (set integer-comparator)
      (set integer-comparator))))

  (test #t (set=? (set integer-comparator 1 2 3 7 8)
    (set-xor!
      (set integer-comparator 1 2 3 4 5 6)
      (set integer-comparator 4 5 6 7 8))))
  (test #t (set=? (set integer-comparator 1)
    (set-xor!
      (set integer-comparator 1 2 3)
      (set integer-comparator 2 3))))
  (test #t (set=? (set integer-comparator 4)
    (set-xor!
      (set integer-comparator 1 2 3)
      (set integer-comparator 1 2 3 4))))
  (test #t (set=? (set integer-comparator)
    (set-xor!
      (set integer-comparator 1)
      (set integer-comparator 1))))
  (test #t (set=? (set integer-comparator 1 2 3)
    (set-xor!
      (set integer-comparator)
      (set integer-comparator 1 2 3))))
  (test #t (set=? (set integer-comparator 4 5 6)
    (set-xor!
      (set integer-comparator 4 5 6)
      (set integer-comparator))))
  (test #t (set=? (set integer-comparator)
    (set-xor!
      (set integer-comparator)
      (set integer-comparator)))))

(test-end)

(test-begin "bags")

(test-group "Constructors & predicates"
  (define empty-bag (bag integer-comparator))
  (test #t (bag? empty-bag))
  (test #t (bag-empty? empty-bag))

  (define full-bag (bag integer-comparator 1 2 3 2 1))
  (test #t (bag? full-bag))
  (test #f (bag-empty? full-bag))
  (test #t (bag-contains? full-bag 1))
  (test #t (bag-contains? full-bag 2))
  (test #t (bag-contains? full-bag 3))
  (test #f (bag-contains? full-bag 4))

  (define test-bag1
    (bag-unfold integer-comparator
      (lambda (n) (< n 0))
      (lambda (n) (* 2 n))
      (lambda (n) (- n 1))
      4))
  (test #t (bag? test-bag1))
  (test #f (bag-empty? test-bag1))
  (test #f (bag-contains? test-bag1 10))
  (test #t (bag-contains? test-bag1 8))
  (test #t (bag-contains? test-bag1 6))
  (test #t (bag-contains? test-bag1 4))
  (test #f (bag-contains? test-bag1 3))
  (test #t (bag-contains? test-bag1 2))
  (test #f (bag-contains? test-bag1 1))
  (test #t (bag-contains? test-bag1 0))
  (test #f (bag-contains? test-bag1 -2))

  (define test-bag2
    (bag-unfold integer-comparator
      (lambda (n) #t)
      (lambda (n) n)
      (lambda (n) (+ n 1))
      4))
  (test #t (bag? test-bag2))
  (test #t (bag-empty? test-bag2))

  (define test-bag3
    (bag-unfold integer-comparator
      (lambda (n) (< n 0))
      (lambda (n) 42)
      (lambda (n) (- n 1))
      3))
  (test #t (bag? test-bag3))
  (test #f (bag-empty? test-bag3))
  (test #f (bag-contains? test-bag3 0))
  (test #f (bag-contains? test-bag3 1))
  (test #f (bag-contains? test-bag3 2))
  (test #f (bag-contains? test-bag3 3))
  (test #t (bag-contains? test-bag3 42))

  (define bag1 (bag integer-comparator 1 2 3))
  (define bag2 (bag integer-comparator 3 4 5))
  (define bag3 (bag integer-comparator 5 6 7))
  (test #f (bag-disjoint? bag1 bag2))
  (test #f (bag-disjoint? bag2 bag3))
  (test #t (bag-disjoint? bag3 bag1))
  (test #t (bag-disjoint? bag1 empty-bag))
  (test #t (bag-disjoint? bag2 empty-bag))
  (test #t (bag-disjoint? bag3 empty-bag))
  (test #f (bag-disjoint? bag1 bag1))
  (test #f (bag-disjoint? bag2 bag2))
  (test #f (bag-disjoint? bag3 bag3))
  (test #t (bag-disjoint? empty-bag empty-bag)))

(test-group "Accessors & the whole bag"
  (define strange-comparator
    (make-comparator integer?
      (lambda (a b) (if (and (even? a) (even? b)) #t (= a b)))
      #f
      (let ((hash (comparator-hash-function integer-comparator)))
        (lambda (n) (if (even? n) 0 (hash n))))))

  (define empty-bag (bag integer-comparator))
  (define full-bag (bag number-comparator 1 2 3 3))
  (define test-bag (bag strange-comparator 0 1 2 3 4 5))

  (test integer-comparator (bag-element-comparator empty-bag))
  (test '() (bag-member empty-bag 42 '()))
  (test '() (bag-member empty-bag 0 '()))

  (test number-comparator (bag-element-comparator full-bag))
  (test  1 (bag-member full-bag 1 #f))
  (test  2 (bag-member full-bag 2 #f))
  (test  3 (bag-member full-bag 3 #f))
  (test #f (bag-member full-bag 4 #f))
  (test  1 (bag-element-count full-bag 1))
  (test  1 (bag-element-count full-bag 2))
  (test  2 (bag-element-count full-bag 3))
  (test  0 (bag-element-count full-bag 4))

  (test strange-comparator (bag-element-comparator test-bag))
  (test #t (= (bag-member test-bag 0 #t)
              (bag-member test-bag 2 #t)
              (bag-member test-bag 4 #t)
              (bag-member test-bag 6 #t)))
  (test  1 (bag-member test-bag 1 #t))
  (test  3 (bag-member test-bag 3 #t))
  (test  5 (bag-member test-bag 5 #t))
  (test #t (bag-member test-bag 7 #t))
  (test  3 (bag-element-count test-bag 0))
  (test  1 (bag-element-count test-bag 1))
  (test  3 (bag-element-count test-bag 2))
  (test  1 (bag-element-count test-bag 3))
  (test  3 (bag-element-count test-bag 4))
  (test  1 (bag-element-count test-bag 5))
  (test  3 (bag-element-count test-bag 6))
  (test  0 (bag-element-count test-bag 7))

  (test 0 (bag-size empty-bag))
  (test 4 (bag-size full-bag))
  (test 6 (bag-size test-bag))

  (test 0 (bag-unique-size empty-bag))
  (test 3 (bag-unique-size full-bag))
  (test 4 (bag-unique-size test-bag))

  (test '() (bag-find even? empty-bag (lambda () '())))
  (test #t (even? (bag-find even? full-bag (lambda () '()))))
  (test #t (odd?  (bag-find odd?  test-bag (lambda () 0))))

  (test 0 (bag-count even? empty-bag))
  (test 1 (bag-count even? full-bag))
  (test 3 (bag-count even? test-bag))

  (test 0 (bag-count odd? empty-bag))
  (test 3 (bag-count odd? full-bag))
  (test 3 (bag-count odd? test-bag))

  (test #f (bag-any? even? empty-bag))
  (test #t (bag-any? even? full-bag))
  (test #t (bag-any? even? test-bag))
  (test #f (bag-any? (lambda (n) (> n 5)) empty-bag))
  (test #f (bag-any? (lambda (n) (> n 5)) full-bag))
  (test #f (bag-any? (lambda (n) (> n 5)) test-bag))

  (test #t (bag-every? even? empty-bag))
  (test #f (bag-every? even? full-bag))
  (test #f (bag-every? even? test-bag))
  (test #t (bag-every? (lambda (n) (< n 10)) empty-bag))
  (test #t (bag-every? (lambda (n) (< n 10)) full-bag))
  (test #t (bag-every? (lambda (n) (< n 10)) test-bag)))

(test-group "Subsets"
  (define empty-bag (bag integer-comparator))
  (define bag1 (bag integer-comparator 1))
  (define bag2 (bag integer-comparator 1 2 3))
  (define bag3 (bag integer-comparator 1 2 3 3))
  (define bag4 (bag integer-comparator 1 2 3 3 4 4 5))
  (define bag5 (bag integer-comparator 3 4 5))
  (define bag6 (bag integer-comparator 5 6 6 6 7 8))
  (define bag7 (bag integer-comparator 5 5 6 6 7 8))
  (define bag8 (bag integer-comparator 5 6 7))
  (define bag9 (bag integer-comparator 3 5))

  (test #t (bag=? empty-bag empty-bag))
  (test #t (bag=? bag1 bag1))
  (test #t (bag=? bag2 (bag integer-comparator 3 2 1)))
  (test #f (bag=? bag1 bag2))
  (test #f (bag=? bag2 bag3))
  (test #t (bag=? (bag integer-comparator 1) (bag integer-comparator 1) (bag integer-comparator 1)))
  (test #f (bag=? bag2 bag8))

  (test #t (bag<? empty-bag bag1 bag2 bag3 bag4))
  (test #t (bag<? bag8 bag6))
  (test #t (bag<? bag8 bag7))
  (test #t (bag<? bag9 bag5))
  (test #t (bag<? bag5 bag4))
  (test #f (bag<? bag6 bag7))
  (test #f (bag<? bag7 bag6))
  (test #f (bag<? bag5 bag6))
  (test #f (bag<? empty-bag empty-bag))
  (test #f (bag<? bag4 bag4))

  (test #t (bag>? bag4 bag3 bag2 bag1 empty-bag))
  (test #t (bag>? bag6 bag8))
  (test #t (bag>? bag7 bag8))
  (test #t (bag>? bag5 bag9))
  (test #t (bag>? bag4 bag5))
  (test #f (bag>? bag7 bag6))
  (test #f (bag>? bag6 bag7))
  (test #f (bag>? bag6 bag5))
  (test #f (bag>? empty-bag empty-bag))
  (test #f (bag>? bag4 bag4))

  (test #t (bag<=? empty-bag bag1 bag2 bag3 bag4))
  (test #t (bag<=? bag8 bag6))
  (test #t (bag<=? bag8 bag7))
  (test #t (bag<=? bag9 bag5))
  (test #t (bag<=? bag5 bag4))
  (test #f (bag<=? bag6 bag7))
  (test #f (bag<=? bag7 bag6))
  (test #f (bag<=? bag5 bag6))
  (test #t (bag<=? empty-bag empty-bag))
  (test #t (bag<=? bag4 bag4))

  (test #t (bag>=? bag4 bag3 bag2 bag1 empty-bag))
  (test #t (bag>=? bag6 bag8))
  (test #t (bag>=? bag7 bag8))
  (test #t (bag>=? bag5 bag9))
  (test #t (bag>=? bag4 bag5))
  (test #f (bag>=? bag7 bag6))
  (test #f (bag>=? bag6 bag7))
  (test #f (bag>=? bag6 bag5))
  (test #t (bag>=? empty-bag empty-bag))
  (test #t (bag>=? bag4 bag4)))

(test-group "Copying and conversion"
  (define bag1 (bag integer-comparator 0 1 9 1))
  (define bag2 (bag-copy bag1))
  (test #t (bag? bag2))
  (test integer-comparator (bag-element-comparator bag2))
  (test #t (bag=? bag1 bag2))
  (test 1 (bag-element-count bag2 0))
  (test 2 (bag-element-count bag2 1))
  (test 1 (bag-element-count bag2 9))

  (test #t (lset= equal? '()
    (bag->list (bag integer-comparator))))
  (test #t (lset= equal? '(1 2 3)
    (bag->list (bag integer-comparator 1 2 3))))
  (test #t (lset= equal? '(1 1 1 1 1 1)
    (bag->list (bag integer-comparator 1 1 1 1 1 1))))

  (test #t (lset= equal? '()
    (bag->alist (bag integer-comparator))))
  (test #t (lset= equal? '((1 . 1) (2 . 1) (3 . 1))
    (bag->alist (bag integer-comparator 1 2 3))))
  (test #t (lset= equal? '((1 . 6))
    (bag->alist (bag integer-comparator 1 1 1 1 1 1))))

  (define bag3 (list->bag integer-comparator '()))
  (test #t (bag? bag3))
  (test #t (bag-empty? bag3))
  (test integer-comparator (bag-element-comparator bag3))

  (define bag4 (list->bag integer-comparator '(1 1 1)))
  (test #t (bag? bag4))
  (test 3 (bag-size bag4))
  (test 1 (bag-unique-size bag4))
  (test 3 (bag-element-count bag4 1))
  (test integer-comparator (bag-element-comparator bag4))

  (define bag5 (list->bag integer-comparator '(1 4 6)))
  (test #t (bag? bag5))
  (test 3 (bag-size bag5))
  (test 1 (bag-element-count bag5 1))
  (test 1 (bag-element-count bag5 4))
  (test 1 (bag-element-count bag5 6))
  (test integer-comparator (bag-element-comparator bag5))

  (define bag6 (alist->bag integer-comparator '()))
  (test #t (bag? bag6))
  (test #t (bag-empty? bag6))
  (test integer-comparator (bag-element-comparator bag6))

  (define bag7 (alist->bag integer-comparator '((1 . 1) (2 . 2) (3 . 9))))
  (test #t (bag? bag7))
  (test 12 (bag-size bag7))
  (test 1 (bag-element-count bag7 1))
  (test 2 (bag-element-count bag7 2))
  (test 9 (bag-element-count bag7 3))
  (test integer-comparator (bag-element-comparator bag7))

  (define bag8 (bag integer-comparator 1 2 3))
  (test #f (bag-contains? bag8 4))

  (define bag9 (list->bag! (bag-copy bag8) '(4 4 5)))
  (test #t (bag-contains? bag9 4))

  (define bag10 (list->bag! (bag-copy bag9) '()))
  (test #t (bag=? bag9 bag10))

  (test #t (lset= equal? '(1 2 3 4 4 5) (bag->list bag10)))

  (test #t (set=? (set integer-comparator 1 2 3)
    (bag->set (bag integer-comparator 1 2 2 3 3))))
  (test #t (set=? (set integer-comparator)
    (bag->set (bag integer-comparator))))
  (test #t (set=? (set integer-comparator 1)
    (bag->set (bag integer-comparator 1 1 1 1 1 1 1))))

  (test #t (bag=? (bag integer-comparator 1 2 3)
    (set->bag (set integer-comparator 1 2 2 3 3))))
  (test #t (bag=? (bag integer-comparator)
    (set->bag (set integer-comparator))))
  (test #t (bag=? (bag integer-comparator 1)
    (set->bag (set integer-comparator 1 1 1 1 1 1 1))))

  (define bag12 (bag integer-comparator 1 2 2 3 3 3))

  (define bag13 (set->bag! (bag-copy bag12) (set integer-comparator)))
  (test #t (bag=? bag12 bag13))

  (define bag14 (set->bag! (bag-copy bag13) (set integer-comparator 1 1 9 9 9)))
  (test #t (bag=? bag14 (bag integer-comparator 1 1 2 2 3 3 3 9)))

  (define bag15 (set->bag! (bag-copy bag14) (set integer-comparator 0 2 3)))
  (test #t (bag=? bag15 (bag integer-comparator 0 1 1 2 2 2 3 3 3 3 9))))

(test-group "Mapping and folding"
  (test #t (bag=? (bag integer-comparator)
    (bag-map integer-comparator
      (lambda (x) x)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator 2 4 6)
    (bag-map integer-comparator
      (lambda (x) (* 2 x))
      (bag integer-comparator 1 2 3))))

  (let ((even-count 0) (invoke-count 0))
    (bag-for-each
      (lambda (x)
        (set! invoke-count (+ 1 invoke-count))
        (if (even? x)
            (set! even-count (+ 1 even-count))
            #f))
      (bag integer-comparator 1 2 3 4 5 2))
    (test 3 even-count)
    (test 6 invoke-count))
  (let ((invoke-count 0))
    (bag-for-each
      (lambda (x) (set! invoke-count (+ 1 invoke-count)))
      (bag integer-comparator))
    (test 0 invoke-count))

  (let ((total-even-count 0) (invoke-count 0))
    (bag-for-each-unique
      (lambda (x count)
        (set! invoke-count (+ 1 invoke-count))
        (if (even? x)
            (set! total-even-count (+ count total-even-count))
            #f))
      (bag integer-comparator 1 2 3 4 5 2))
    (test 3 total-even-count)
    (test 5 invoke-count))
  (let ((invoke-count 0))
    (bag-for-each-unique
      (lambda (x count) (set! invoke-count (+ 1 invoke-count)))
      (bag integer-comparator))
    (test 0 invoke-count))

  (test 64 (bag-fold + 0 (bag integer-comparator 5 3 56)))
  (test #f (bag-fold + #f (bag integer-comparator)))

  (test 38 (bag-fold-unique
    (lambda (elt count sum)
      (+ sum (* elt count)))
    0
    (bag integer-comparator 1 2 3 2 3 5 4 8 2 1 4 3)))
  (test #f (bag-fold-unique
    (lambda (elt count sum)
      (+ sum (* elt count)))
    #f
    (bag integer-comparator)))

  (test #t (bag=? (bag integer-comparator 2 4 4 6)
    (bag-filter even? (bag integer-comparator 1 2 3 4 5 6 7 4))))
  (test #t (bag=? (bag integer-comparator)
    (bag-filter even? (bag integer-comparator 1 5 7))))
  (test #t (bag=? (bag integer-comparator)
    (bag-filter (lambda (x) #t) (bag integer-comparator))))

  (test #t (bag=? (bag integer-comparator 2 4 4 6)
    (bag-filter! even? (bag integer-comparator 1 2 3 4 5 6 7 4))))
  (test #t (bag=? (bag integer-comparator)
    (bag-filter! even? (bag integer-comparator 1 5 7))))
  (test #t (bag=? (bag integer-comparator)
    (bag-filter! (lambda (x) #t) (bag integer-comparator))))

  (test #t (bag=? (bag integer-comparator 5 6)
    (bag-remove (lambda (x) (< x 5)) (bag integer-comparator 1 2 3 2 4 5 6))))
  (test #t (bag=? (bag integer-comparator)
    (bag-remove (lambda (x) (< x 5)) (bag integer-comparator -1 0 0 +1))))
  (test #t (bag=? (bag integer-comparator 8 9 10 10)
    (bag-remove (lambda (x) (< x 5)) (bag integer-comparator 8 9 10 10))))

  (test #t (bag=? (bag integer-comparator 5 6)
    (bag-remove! (lambda (x) (< x 5)) (bag integer-comparator 1 2 3 4 5 6))))
  (test #t (bag=? (bag integer-comparator)
    (bag-remove! (lambda (x) (< x 5)) (bag integer-comparator -1 0 +1))))
  (test #t (bag=? (bag integer-comparator 8 9 10)
    (bag-remove! (lambda (x) (< x 5)) (bag integer-comparator 8 9 10))))

  (call-with-values
    (lambda () (bag-partition number? (bag integer-comparator)))
    (lambda (nope1 nope2)
      (test #t (bag-empty? nope1))
      (test #t (bag-empty? nope2))))
  (call-with-values
    (lambda () (bag-partition even? (bag integer-comparator 1 2 3 4 2 3 4 5)))
    (lambda (even odd)
      (test #t (bag=? even (bag integer-comparator 2 4 2 4)))
      (test #t (bag=? odd (bag integer-comparator 1 3 5 3)))))
  (call-with-values
    (lambda () (bag-partition (lambda (n) (< n 10)) (bag integer-comparator 1 2 3 4 5 4 5)))
    (lambda (small big)
      (test #t (bag=? small (bag integer-comparator 1 2 3 4 5 4 5)))
      (test #t (bag-empty? big))))

  (call-with-values
    (lambda () (bag-partition! number? (bag integer-comparator)))
    (lambda (nope1 nope2)
      (test #t (bag-empty? nope1))
      (test #t (bag-empty? nope2))))
  (call-with-values
    (lambda () (bag-partition! even? (bag integer-comparator 1 2 3 4 2 3 4 5)))
    (lambda (even odd)
      (test #t (bag=? even (bag integer-comparator 2 4 2 4)))
      (test #t (bag=? odd (bag integer-comparator 1 3 5 3)))))
  (call-with-values
    (lambda () (bag-partition! (lambda (n) (< n 10)) (bag integer-comparator 1 2 3 4 5 4 5)))
    (lambda (small big)
      (test #t (bag=? small (bag integer-comparator 1 2 3 4 5 4 5)))
      (test #t (bag-empty? big)))))

(test-group "Updaters"
  (define bag1 (bag integer-comparator 1 2 3 3))
  (define bag2 (bag-adjoin bag1 4 5 5 5 2))
  (test #t (bag? bag2))
  (test #t (bag=? bag2 (bag integer-comparator 1 2 2 3 3 4 5 5 5)))

  (define bag3 (bag-adjoin! bag1 1 2 6))
  (test #t (bag=? bag3 (bag integer-comparator 1 1 2 2 3 3 6)))

  (define strange-comparator
    (make-comparator integer?
      (lambda (a b) (if (and (even? a) (even? b)) #t (= a b)))
      #f
      (let ((hash (comparator-hash-function integer-comparator)))
        (lambda (n) (if (even? n) 0 (hash n))))))

  (define bag4 (bag strange-comparator 0 1 2 3 4))
  (test #t (bag=? bag4 (bag strange-comparator 0 2 4 1 3)))
  (test #t (bag=? bag4 (bag strange-comparator 2 2 2 1 3)))
  (test #t (bag=? bag4 (bag strange-comparator 0 0 4 1 3)))
  (test 0 (bag-member bag4 0 #f))
  (test 0 (bag-member bag4 2 #f))
  (test 0 (bag-member bag4 4 #f))
  (test 3 (bag-element-count bag4 0))
  (test 3 (bag-element-count bag4 2))
  (test 3 (bag-element-count bag4 4))
  (test 1 (bag-element-count bag4 1))
  (test 1 (bag-element-count bag4 3))

  (define bag5 (bag-replace (bag strange-comparator 0 1 2 3 4) 4))
  (test #t (bag=? bag5 (bag strange-comparator 0 2 4 1 3)))
  (test #t (bag=? bag5 (bag strange-comparator 2 2 2 1 3)))
  (test #t (bag=? bag5 (bag strange-comparator 0 0 4 1 3)))
  (test 4 (bag-member bag5 0 #f))
  (test 4 (bag-member bag5 2 #f))
  (test 4 (bag-member bag5 4 #f))
  (test 3 (bag-element-count bag5 0))
  (test 3 (bag-element-count bag5 2))
  (test 3 (bag-element-count bag5 4))
  (test 1 (bag-element-count bag5 1))
  (test 1 (bag-element-count bag5 3))

  (define bag6 (bag-replace! (bag strange-comparator 0 1 2 3 4) 2))
  (test #t (bag=? bag5 (bag strange-comparator 0 2 4 1 3)))
  (test #t (bag=? bag5 (bag strange-comparator 2 2 2 1 3)))
  (test #t (bag=? bag5 (bag strange-comparator 0 0 4 1 3)))
  (test 2 (bag-member bag6 0 #f))
  (test 2 (bag-member bag6 2 #f))
  (test 2 (bag-member bag6 4 #f))
  (test 3 (bag-element-count bag6 0))
  (test 3 (bag-element-count bag6 2))
  (test 3 (bag-element-count bag6 4))
  (test 1 (bag-element-count bag6 1))
  (test 1 (bag-element-count bag6 3))

  (define bag7 (bag integer-comparator 1 5 8 8 9 9))

  (test #t (bag=? (bag integer-comparator 1 5)
    (bag-delete bag7 8 9)))
  (test #t (bag=? (bag integer-comparator 1 5 9 9)
    (bag-delete bag7 8 10 11 47)))
  (test #t (bag-empty? (bag-delete bag7 1 5 8 9 5 9 1)))

  (test #t (bag=? (bag integer-comparator 1 5)
    (bag-delete! (bag-copy bag7) 8 9)))
  (test #t (bag=? (bag integer-comparator 1 5 9 9)
    (bag-delete! (bag-copy bag7) 8 10 11 47)))
  (test #t (bag-empty? (bag-delete! (bag-copy bag7) 1 5 8 9 5 9 1)))

  (test #t (bag=? (bag integer-comparator 1 5)
    (bag-delete-all bag7 '(8 9))))
  (test #t (bag=? (bag integer-comparator 1 5 9 9)
    (bag-delete-all bag7 '(8 10 11 47))))
  (test #t (bag-empty? (bag-delete-all bag7 '(1 5 8 9 5 9 1))))

  (test #t (bag=? (bag integer-comparator 1 5)
    (bag-delete-all! (bag-copy bag7) '(8 9))))
  (test #t (bag=? (bag integer-comparator 1 5 9 9)
    (bag-delete-all! (bag-copy bag7) '(8 10 11 47))))
  (test #t (bag-empty? (bag-delete-all! (bag-copy bag7) '(1 5 8 9 5 9 1))))

  (define bag8 (bag integer-comparator 5 8 8 1 1))

  (call-with-values
    (lambda ()
      (bag-search! (bag-copy bag8) 0
        (lambda (insert ignore)
          (insert 'insert))
        (lambda (element update remove) #f)))
    (lambda (bag8 obj)
      (test 'insert obj)
      (test #t (bag=? bag8 (bag integer-comparator 5 8 1 0 8 1)))))

  (call-with-values
    (lambda ()
      (bag-search! (bag-copy bag8) 0
        (lambda (insert ignore) (ignore 'ignore))
        (lambda (element update remove) #f)))
    (lambda (bag8 obj)
      (test 'ignore obj)
      (test #t (bag=? bag8 (bag integer-comparator 5 8 1 8 1)))))

  (call-with-values
    (lambda ()
      (bag-search! (bag-copy bag8) 8
        (lambda (insert ignore) #f)
        (lambda (element update remove)
          (test 8 element)
          (update 9 'update))))
    (lambda (bag8 obj)
      (test 'update obj)
      (test #t (bag=? bag8 (bag integer-comparator 5 9 1 8 1)))))

  (call-with-values
    (lambda ()
      (bag-search! (bag-copy bag8) 1
        (lambda (insert ignore) #f)
        (lambda (element update remove)
          (test 1 element)
          (remove 'remove))))
    (lambda (bag8 obj)
      (test 'remove obj)
      (test #t (bag=? bag8 (bag integer-comparator 5 8 8 1)))))

  (define bag10 (alist->bag integer-comparator '((1 . 1) (2 . 3) (3 . 5))))

  (define bag11 (bag-increment! (bag-copy bag10) 1 2))
  (test #t (bag=? bag11 (alist->bag integer-comparator '((1 . 3) (2 . 3) (3 . 5)))))

  (define bag12 (bag-increment! (bag-copy bag11) 2 6))
  (test #t (bag=? bag12 (alist->bag integer-comparator '((1 . 3) (2 . 9) (3 . 5)))))

  (define bag13 (bag-increment! (bag-copy bag12) 9 2))
  (test #t (bag=? bag13 (alist->bag integer-comparator '((1 . 3) (2 . 9) (3 . 5) (9 . 2)))))

  (define bag14 (bag-decrement! (bag-copy bag13) 3 3))
  (test #t (bag=? bag14 (alist->bag integer-comparator '((1 . 3) (2 . 9) (3 . 2) (9 . 2)))))

  (define bag15 (bag-decrement! (bag-copy bag14) 1 3))
  (test #t (bag=? bag15 (alist->bag integer-comparator '((2 . 9) (3 . 2) (9 . 2)))))

  (define bag16 (bag-decrement! (bag-copy bag15) 9 500))
  (test #t (bag=? bag16 (alist->bag integer-comparator '((2 . 9) (3 . 2))))))

(test-group "Set theory operations"
  (test #t (bag=? (bag integer-comparator 1 2 3 4 5 6 6 7)
    (bag-union
      (bag integer-comparator 1 2)
      (bag integer-comparator 2 3)
      (bag integer-comparator 4)
      (bag integer-comparator 6 5 7)
      (bag integer-comparator 6 6))))
  (test #t (bag=? (bag integer-comparator 5 5 5)
    (bag-union
      (bag integer-comparator 5)
      (bag integer-comparator 5 5 5)
      (bag integer-comparator)
      (bag integer-comparator 5))))
  (test #t (bag=? (bag integer-comparator)
    (bag-union
      (bag integer-comparator)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator 1 1 2 3)
    (bag-union
      (bag integer-comparator 1 2 3 1))))

  (test #t (bag=? (bag integer-comparator 1 2 3 4 5 6 6 7)
    (bag-union!
      (bag integer-comparator 1 2)
      (bag integer-comparator 2 3)
      (bag integer-comparator 4)
      (bag integer-comparator 6 5 7)
      (bag integer-comparator 6 6))))
  (test #t (bag=? (bag integer-comparator 5 5 5)
    (bag-union!
      (bag integer-comparator 5)
      (bag integer-comparator 5 5 5)
      (bag integer-comparator)
      (bag integer-comparator 5))))
  (test #t (bag=? (bag integer-comparator)
    (bag-union!
      (bag integer-comparator)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator 1 1 2 3)
    (bag-union!
      (bag integer-comparator 1 2 3 1))))

  (test #t (bag=? (bag integer-comparator)
    (bag-intersection
      (bag integer-comparator)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator)
    (bag-intersection
      (bag integer-comparator 1 2 3)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator)
    (bag-intersection
      (bag integer-comparator 1 2)
      (bag integer-comparator 3 4))))
  (test #t (bag=? (bag integer-comparator 4 4)
    (bag-intersection
      (bag integer-comparator 1 2 3 3 4 4)
      (bag integer-comparator 3 4 4)
      (bag integer-comparator 4 4 5 6))))
  (test #t (bag=? (bag integer-comparator 2 1 3)
    (bag-intersection
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 3 2 1 1))))
  (test #t (bag=? (bag integer-comparator 1 1 2 3)
    (bag-intersection
      (bag integer-comparator 1 2 3 1))))

  (test #t (bag=? (bag integer-comparator)
    (bag-intersection!
      (bag integer-comparator)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator)
    (bag-intersection!
      (bag integer-comparator 1 2 3)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator)
    (bag-intersection!
      (bag integer-comparator 1 2)
      (bag integer-comparator 3 4))))
  (test #t (bag=? (bag integer-comparator 4 4)
    (bag-intersection!
      (bag integer-comparator 1 2 3 3 4 4)
      (bag integer-comparator 3 4 4)
      (bag integer-comparator 4 4 5 6))))
  (test #t (bag=? (bag integer-comparator 2 1 3)
    (bag-intersection!
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 3 2 1 1))))
  (test #t (bag=? (bag integer-comparator 1 1 2 3)
    (bag-intersection!
      (bag integer-comparator 1 2 3 1))))

  (test #t (bag=? (bag integer-comparator 2)
    (bag-difference
      (bag integer-comparator 1 2 3 4 5)
      (bag integer-comparator 3 4)
      (bag integer-comparator 5)
      (bag integer-comparator 1))))
  (test #t (bag=? (bag integer-comparator 1 2 3 4 5)
    (bag-difference
      (bag integer-comparator 1 2 3 4 5))))
  (test #t (bag=? (bag integer-comparator)
    (bag-difference
      (bag integer-comparator 1 2 3 4 5)
      (bag integer-comparator 3 4 5)
      (bag integer-comparator 1 2 3))))
  (test #t (bag=? (bag integer-comparator 2)
    (bag-difference
      (bag integer-comparator 1 2 3 4 5)
      (bag integer-comparator 3 4 5 6 7 8 9)
      (bag integer-comparator 1))))
  (test #t (bag=? (bag integer-comparator)
    (bag-difference
      (bag integer-comparator)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator)
    (bag-difference
      (bag integer-comparator)
      (bag integer-comparator 4))))
  (test #t (bag=? (bag integer-comparator 3 4 4 5)
    (bag-difference
      (bag integer-comparator 1 2 2 3 3 4 4 4 5 5)
      (bag integer-comparator 3 4)
      (bag integer-comparator 5 2 2 2 2)
      (bag integer-comparator 1))))

  (test #t (bag=? (bag integer-comparator 2)
    (bag-difference!
      (bag integer-comparator 1 2 3 4 5)
      (bag integer-comparator 3 4)
      (bag integer-comparator 5)
      (bag integer-comparator 1))))
  (test #t (bag=? (bag integer-comparator 1 2 3 4 5)
    (bag-difference!
      (bag integer-comparator 1 2 3 4 5))))
  (test #t (bag=? (bag integer-comparator)
    (bag-difference!
      (bag integer-comparator 1 2 3 4 5)
      (bag integer-comparator 3 4 5)
      (bag integer-comparator 1 2 3))))
  (test #t (bag=? (bag integer-comparator 2)
    (bag-difference!
      (bag integer-comparator 1 2 3 4 5)
      (bag integer-comparator 3 4 5 6 7 8 9)
      (bag integer-comparator 1))))
  (test #t (bag=? (bag integer-comparator)
    (bag-difference!
      (bag integer-comparator)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator)
    (bag-difference!
      (bag integer-comparator)
      (bag integer-comparator 4))))
  (test #t (bag=? (bag integer-comparator 3 4 4 5)
    (bag-difference!
      (bag integer-comparator 1 2 2 3 3 4 4 4 5 5)
      (bag integer-comparator 3 4)
      (bag integer-comparator 5 2 2 2 2)
      (bag integer-comparator 1))))

  (test #t (bag=? (bag integer-comparator 1 2 2 3 7 8)
    (bag-xor
      (bag integer-comparator 1 2 2 3 4 5 6)
      (bag integer-comparator 4 5 6 7 8))))
  (test #t (bag=? (bag integer-comparator 1 2)
    (bag-xor
      (bag integer-comparator 1 2 2 3)
      (bag integer-comparator 2 3))))
  (test #t (bag=? (bag integer-comparator 1 2)
    (bag-xor
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 2 2 3))))
  (test #t (bag=? (bag integer-comparator 4)
    (bag-xor
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 1 2 3 4))))
  (test #t (bag=? (bag integer-comparator)
    (bag-xor
      (bag integer-comparator 1)
      (bag integer-comparator 1))))
  (test #t (bag=? (bag integer-comparator 1 1 1)
    (bag-xor
      (bag integer-comparator 1)
      (bag integer-comparator 1 1 1 1))))
  (test #t (bag=? (bag integer-comparator 1 2 3)
    (bag-xor
      (bag integer-comparator)
      (bag integer-comparator 1 2 3))))
  (test #t (bag=? (bag integer-comparator 4 5 6)
    (bag-xor
      (bag integer-comparator 4 5 6)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator)
    (bag-xor
      (bag integer-comparator)
      (bag integer-comparator))))

  (test #t (bag=? (bag integer-comparator 1 2 2 3 7 8)
    (bag-xor!
      (bag integer-comparator 1 2 2 3 4 5 6)
      (bag integer-comparator 4 5 6 7 8))))
  (test #t (bag=? (bag integer-comparator 1 2)
    (bag-xor!
      (bag integer-comparator 1 2 2 3)
      (bag integer-comparator 2 3))))
  (test #t (bag=? (bag integer-comparator 1 2)
    (bag-xor!
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 2 2 3))))
  (test #t (bag=? (bag integer-comparator 4)
    (bag-xor!
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 1 2 3 4))))
  (test #t (bag=? (bag integer-comparator)
    (bag-xor!
      (bag integer-comparator 1)
      (bag integer-comparator 1))))
  (test #t (bag=? (bag integer-comparator 1 1 1)
    (bag-xor!
      (bag integer-comparator 1)
      (bag integer-comparator 1 1 1 1))))
  (test #t (bag=? (bag integer-comparator 1 2 3)
    (bag-xor!
      (bag integer-comparator)
      (bag integer-comparator 1 2 3))))
  (test #t (bag=? (bag integer-comparator 4 5 6)
    (bag-xor!
      (bag integer-comparator 4 5 6)
      (bag integer-comparator))))
  (test #t (bag=? (bag integer-comparator)
    (bag-xor!
      (bag integer-comparator)
      (bag integer-comparator))))

  (test #t (bag=? (bag integer-comparator 1 1 2 3 3 3)
    (bag-sum
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 1)
      (bag integer-comparator 3 3))))
  (test #t (bag=? (bag integer-comparator 1 1)
    (bag-sum
      (bag integer-comparator 1 1))))
  (test #t (bag=? (bag integer-comparator)
    (bag-sum
      (bag integer-comparator) (bag integer-comparator)
      (bag integer-comparator) (bag integer-comparator))))

  (test #t (bag=? (bag integer-comparator 1 1 2 3 3 3)
    (bag-sum!
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 1)
      (bag integer-comparator 3 3))))
  (test #t (bag=? (bag integer-comparator 1 1)
    (bag-sum!
      (bag integer-comparator 1 1))))
  (test #t (bag=? (bag integer-comparator)
    (bag-sum!
      (bag integer-comparator) (bag integer-comparator)
      (bag integer-comparator) (bag integer-comparator))))

  (test #t (bag=? (bag integer-comparator)
    (bag-product 9 (bag integer-comparator))))
  (test #t (bag=? (alist->bag integer-comparator '((1 . 2) (2 . 4) (3 . 6)))
    (bag-product 2 (bag integer-comparator 1 2 2 3 3 3))))
  (test #t (bag=? (bag integer-comparator 1 1 1)
    (bag-product 1 (bag integer-comparator 1 1 1))))
  (test #t (bag=? (bag integer-comparator)
    (bag-product 0 (bag integer-comparator 1 1 1 2 2))))

  (test #t (bag=? (bag integer-comparator)
    (bag-product! 9 (bag integer-comparator))))
  (test #t (bag=? (alist->bag integer-comparator '((1 . 2) (2 . 4) (3 . 6)))
    (bag-product! 2 (bag integer-comparator 1 2 2 3 3 3))))
  (test #t (bag=? (bag integer-comparator 1 1 1)
    (bag-product! 1 (bag integer-comparator 1 1 1))))
  (test #t (bag=? (bag integer-comparator)
    (bag-product! 0 (bag integer-comparator 1 1 1 2 2)))))

(test-end)

(test-begin "Comparators")

(test-group "Basic"
  (test #t (comparator? set-comparator))
  (test #t (comparator? bag-comparator))
  (test #f (comparator-comparison-procedure? set-comparator))
  (test #f (comparator-comparison-procedure? bag-comparator))
  (test #t (comparator-hash-function? set-comparator))
  (test #t (comparator-hash-function? bag-comparator))

  (test #t (comparator-test-type set-comparator (set integer-comparator)))
  (test #f (comparator-test-type set-comparator (bag integer-comparator)))
  (test #f (comparator-test-type set-comparator '(1 2 3)))
  (test #f (comparator-test-type set-comparator '((1 . 1) (2 . 2))))
  (test #f (comparator-test-type set-comparator 42))

  (test #t (comparator-test-type bag-comparator (bag integer-comparator)))
  (test #f (comparator-test-type bag-comparator (set integer-comparator)))
  (test #f (comparator-test-type bag-comparator '(1 2 3)))
  (test #f (comparator-test-type bag-comparator '((1 . 1) (2 . 2))))
  (test #f (comparator-test-type bag-comparator 42))

  (test #t (=? set-comparator (set integer-comparator 1 2 3)
                              (set integer-comparator 3 2 1)))

  (test #f (=? bag-comparator (bag integer-comparator 1 2 3 3)
                              (bag integer-comparator 3 2 1))))

(test-group "Basic hashing"
  (test #t (= (comparator-hash set-comparator (set integer-comparator))
              (comparator-hash set-comparator (set integer-comparator))))

  (test #t (= (comparator-hash set-comparator (set integer-comparator 1 2 3))
              (comparator-hash set-comparator (set integer-comparator 3 2 1 1 1))))

  (test #t (= (comparator-hash set-comparator (set integer-comparator 1 2 3))
              (comparator-hash set-comparator (set integer-comparator 3 2 1))))

  (test #t (= (comparator-hash set-comparator (set integer-comparator 1 4 2 3))
              (comparator-hash set-comparator (set integer-comparator 3 2 2 2 1 4))))

  (test #t (= (comparator-hash bag-comparator (bag integer-comparator))
              (comparator-hash bag-comparator (bag integer-comparator))))

  (test #t (= (comparator-hash bag-comparator (bag integer-comparator 1 2 3 1 1))
              (comparator-hash bag-comparator (bag integer-comparator 3 2 1 1 1))))

  (test #t (= (comparator-hash bag-comparator (bag integer-comparator 8 3 4 6 2 3 4 5 4 3 2 5 4 6 7 5 3 2 4))
              (comparator-hash bag-comparator (alist->bag integer-comparator '((4 . 5) (6 . 2) (2 . 3) (8 . 1) (3 . 4) (5 . 3) (7 . 1))))))

  (test #t (= (comparator-hash bag-comparator (bag integer-comparator -1 -1 0 0 +1 +1))
              (comparator-hash bag-comparator (bag integer-comparator +1 0 -1 +1 0 -1)))))

(test-group "Compound"
  (define set-of-sets
    (set set-comparator
      (set integer-comparator 1 2 3)
      (set string-comparator "1" "2" "3")
      (set set-comparator)))

  (define set-of-bags
    (set bag-comparator
      (bag integer-comparator 1 2 2 3)
      (bag integer-comparator 1 2 3)
      (bag set-comparator
        (set integer-comparator 1)
        (set integer-comparator 1)
        (set integer-comparator 2 2))))

  (define bag-of-sets
    (bag set-comparator
      (set integer-comparator 1 2 3)
      (set integer-comparator 1 2 3)))

  (define bag-of-bags
    (bag bag-comparator
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 1 2 3 3)))

  (test #t (set-contains? set-of-sets (set set-comparator)))
  (test #f (set-contains? set-of-sets (set symbol-comparator 'foo 'bar)))

  (test #f (set-contains? set-of-bags (bag integer-comparator 1 2 2 2 3)))
  (test #t (set-contains? set-of-bags (bag integer-comparator 3 2 2 1)))

  (test 0 (bag-element-count bag-of-sets (set integer-comparator)))
  (test 2 (bag-element-count bag-of-sets (set integer-comparator 1 1 2 3 3)))

  (test 0 (bag-element-count bag-of-bags (bag integer-comparator 1 2)))
  (test 1 (bag-element-count bag-of-bags (bag integer-comparator 1 2 3)))
  (test 1 (bag-element-count bag-of-bags (bag integer-comparator 1 2 3 3)))
  (test 0 (bag-element-count bag-of-bags (bag integer-comparator 1 2 3 3 3))))

(test-group "Compound hashing"
  (test #t (= (comparator-hash set-comparator (set set-comparator))
              (comparator-hash set-comparator (set set-comparator))))

  (test #t (= (comparator-hash set-comparator (set bag-comparator))
              (comparator-hash set-comparator (set bag-comparator))))

  (test #t (= (comparator-hash bag-comparator (bag set-comparator))
              (comparator-hash bag-comparator (bag set-comparator))))

  (test #t (= (comparator-hash bag-comparator (bag bag-comparator))
              (comparator-hash bag-comparator (bag bag-comparator))))

  (define set-of-sets-1
    (set set-comparator
      (set integer-comparator 1 2 3)
      (set string-comparator "1" "2" "3")
      (set set-comparator)))

  (define set-of-sets-2
    (set set-comparator
      (set string-comparator "1" "2" "3")
      (set integer-comparator 1 2 3)
      (set set-comparator)))

  (define set-of-sets-3
    (set set-comparator
      (set string-comparator "1" "2" "3")
      (set integer-comparator 1 2 3)
      (set set-comparator)
      (set integer-comparator 1 2 3)
      (set integer-comparator 1 2 3)
      (set set-comparator)
      (set string-comparator "1" "2" "3")))

  (test #t (= (comparator-hash set-comparator set-of-sets-1)
              (comparator-hash set-comparator set-of-sets-2)
              (comparator-hash set-comparator set-of-sets-3)))

  (define set-of-bags-1
    (set bag-comparator
      (bag integer-comparator 1 2 2 3)
      (bag integer-comparator 1 2 3)
      (bag set-comparator
        (set integer-comparator 1)
        (set integer-comparator 1)
      (set integer-comparator 2 2))))

  (define set-of-bags-2
    (set bag-comparator
      (bag integer-comparator 3 1 2 2)
      (bag integer-comparator 1 2 3)
      (bag set-comparator
        (set integer-comparator 1)
        (set integer-comparator 2 2 2 2 2 2 2)
        (set integer-comparator 1))
      (bag integer-comparator 3 2 1 2)))

  (define set-of-bags-3
    (set bag-comparator
      (bag integer-comparator 1 2 2 3)
      (alist->bag set-comparator
        `((,(set integer-comparator 1) . 2)
          (,(set integer-comparator 2) . 1)))
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 3 2 1)
      (bag integer-comparator 3 1 2)))

  (test #t (= (comparator-hash set-comparator set-of-bags-1)
              (comparator-hash set-comparator set-of-bags-2)
              (comparator-hash set-comparator set-of-bags-3)))

  (define bag-of-sets-1
    (bag set-comparator
      (set integer-comparator 1 2 3)
      (set integer-comparator 1 2 3)))

  (define bag-of-sets-2
    (bag set-comparator
      (set integer-comparator 1 2 1 2 3 3 3)
      (set integer-comparator 3 2 1 2 2 2 1)))

  (define bag-of-sets-3
    (alist->bag set-comparator
      `((,(set integer-comparator 1 2 3 2 1) . 2))))

  (test #t (= (comparator-hash bag-comparator bag-of-sets-1)
              (comparator-hash bag-comparator bag-of-sets-2)
              (comparator-hash bag-comparator bag-of-sets-3)))

  (define bag-of-bags-1
    (bag bag-comparator
      (bag integer-comparator 1 2 3)
      (bag integer-comparator 1 2 3 3)
      (bag integer-comparator 3 2 1)
      (bag set-comparator
        (set integer-comparator 4 5 7 9)
        (set integer-comparator 5 9 5 5 4 7 7)
        (set bag-comparator
          (bag integer-comparator 4 4 4 5 6)
          (bag integer-comparator 2 2 1)))
      (bag set-comparator
        (set integer-comparator 5 5 4 9 5 7)
        (set integer-comparator 5 4 5 7 9)
        (set bag-comparator
          (bag integer-comparator 4 4 4 5 6)
          (bag integer-comparator 2 2 1)
          (alist->bag integer-comparator '((4 . 3) (5 . 1) (6 . 1)))))))

  (define bag-of-bags-2
    (bag bag-comparator
      (bag integer-comparator 2 3 1)
      (bag set-comparator
        (set integer-comparator 4 4 5 7 9 7)
        (set bag-comparator
          (bag integer-comparator 4 4 4 5 6)
          (bag integer-comparator 4 5 4 4 6)
          (bag integer-comparator 6 4 4 4 5)
          (alist->bag integer-comparator '((4 . 3) (5 . 1) (6 . 1)))
          (bag integer-comparator 2 2 1))
        (set integer-comparator 5 9 5 4 7))
      (bag integer-comparator 1 2 3)
      (bag set-comparator
        (set integer-comparator 5 4 5 7 9)
        (set bag-comparator
          (bag integer-comparator 4 4 4 5 6)
          (bag integer-comparator 2 2 1)
          (alist->bag integer-comparator '((4 . 3) (5 . 1) (6 . 1))))
        (set integer-comparator 5 5 4 9 5 7))
      (bag integer-comparator 1 2 3 3)))

  (test #t (= (comparator-hash bag-comparator bag-of-bags-1)
              (comparator-hash bag-comparator bag-of-bags-2))))

(test-group "Default"
  (define set1 (set integer-comparator 1 2 3))
  (define set2 (set integer-comparator 4 5 6))
  (define set3 (set integer-comparator 3 2 1))
  (define bag1 (bag integer-comparator 1 2 3 1 2 3))
  (define bag2 (bag integer-comparator 4 5 6 5 6))
  (define bag3 (alist->bag integer-comparator '((2 . 2) (3 . 2) (1 . 2))))

  (test #t (comparator-equal? default-comparator set1 set1))
  (test #f (comparator-equal? default-comparator set1 set2))
  (test #t (comparator-equal? default-comparator set1 set3))
  (test #f (comparator-equal? default-comparator set2 set3))
  (test #t (comparator-equal? default-comparator bag1 bag1))
  (test #f (comparator-equal? default-comparator bag1 bag2))
  (test #t (comparator-equal? default-comparator bag1 bag3))
  (test #f (comparator-equal? default-comparator bag2 bag3))

  (test #t (= (comparator-hash default-comparator set1)
              (comparator-hash default-comparator set3)))
  (test #t (= (comparator-hash default-comparator bag1)
              (comparator-hash default-comparator bag3)))

  (define list1 (list set1 set2))
  (define list2 (list set1 set3))
  (define list3 (list set3 set2))
  (define list4 (list bag1 bag2))
  (define list5 (list bag1 bag3))
  (define list6 (list bag3 bag2))

  ; list-comparator uses default-comparator to compare elements
  (test #f (comparator-equal? list-comparator list1 list2))
  (test #t (comparator-equal? list-comparator list1 list3))
  (test #f (comparator-equal? list-comparator list1 list4))
  (test #f (comparator-equal? list-comparator list1 list5))
  (test #f (comparator-equal? list-comparator list1 list6))
  (test #f (comparator-equal? list-comparator list2 list3))
  (test #f (comparator-equal? list-comparator list2 list4))
  (test #f (comparator-equal? list-comparator list2 list5))
  (test #f (comparator-equal? list-comparator list2 list6))
  (test #f (comparator-equal? list-comparator list3 list4))
  (test #f (comparator-equal? list-comparator list3 list5))
  (test #f (comparator-equal? list-comparator list3 list6))
  (test #f (comparator-equal? list-comparator list4 list5))
  (test #t (comparator-equal? list-comparator list4 list6))
  (test #f (comparator-equal? list-comparator list5 list6))

  (test #t (= (comparator-hash default-comparator list1)
              (comparator-hash default-comparator list3)))
  (test #t (= (comparator-hash default-comparator list4)
              (comparator-hash default-comparator list6))))

(test-end)

(test-end)

(test-exit)
