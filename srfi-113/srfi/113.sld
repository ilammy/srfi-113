(define-library (srfi 113)
  (export set set-contains? set-unfold set? set-empty? set-disjoint?
   set-member set-element-comparator set-adjoin set-adjoin! set-replace
   set-replace! set-delete set-delete! set-delete-all set-delete-all!
   set-search! set-size set-find set-count set-any? set-every? set-map
   set-for-each set-fold set-filter set-filter! set-remove set-remove!
   set-partition set-partition! set-copy set->list list->set list->set!
   set=? set<? set>? set<=? set>=? set-union set-intersection set-xor
   set-difference set-union! set-intersection! set-difference! set-xor!
   bag bag-contains? bag-unfold bag? bag-empty? bag-disjoint? bag-member
   bag-element-count bag-element-comparator bag-adjoin bag-adjoin!
   bag-replace bag-replace! bag-delete bag-delete! bag-delete-all
   bag-delete-all! bag-search! bag-increment! bag-decrement! bag-size
   bag-unique-size bag-find bag-count bag-any? bag-every? bag-map
   bag-for-each bag-for-each-unique bag-fold bag-fold-unique bag-filter
   bag-filter! bag-remove bag-remove! bag-partition bag-partition!
   bag-sum bag-sum! bag-product bag-product! bag-copy bag->list
   list->bag list->bag! bag->alist alist->bag bag->set set->bag
   set->bag! bag=? bag<? bag>? bag<=? bag>=? bag-union bag-intersection
   bag-difference bag-xor bag-union! bag-intersection! bag-difference!
   bag-xor! set-comparator bag-comparator)

  (import (scheme base)
          (scheme case-lambda)
          (srfi 60)
          (srfi 69)
          (srfi 114))

  (include "113/qmaps.scm"
           "113/qmap-library.scm"
           "113/sets-and-bags.scm"))
