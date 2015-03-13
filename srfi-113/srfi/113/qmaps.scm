;; qmaps.scm -- essentials of quantity maps used by sets and bags
;; Copyright (c) 2015 ilammy <a.lozovsky@gmail.com>
;; 3-clause BSD license: http://github.com/ilammy/srfi-113/blob/master/LICENSE

;; This file contains definitions of essential procedures that implement
;; quantity maps used to implement sets and and bags. You need to override
;; only these procedures to use a different backend. The procedures are
;; extensively documented so there should be no issues with that.
;;
;; This implementation of qmaps relies on SRFI 69 hash tables.

;! Creates an empty qmap.
;;
;; \param [in] comparator  A SRFI 114 comparator.
;;
;; \returns a newly allocated empty qmap.
(define (make-qmap comparator)
  (make-hash-table
    (comparator-equality-predicate comparator)
    ; SRFI 114 defines only one-argument hash functions while SRFI 69 requires
    ; support of both forms. Duh...
    (let ((hash (comparator-hash-function comparator)))
      (case-lambda
        ((object)       (hash object))
        ((object bound) (modulo (hash object) bound))))))

;! Copies an existing qmap.
;;
;; \param [in] qmap  A qmap.
;;
;; \returns a newly allocated copy of #qmap.
(define (qmap-copy qmap)
  (hash-table-copy qmap))

;! Checks whether an object is a qmap.
;;
;; \param [in] object  An arbitrary Scheme object.
;;
;; \returns \#t if #object is a qmap, \#f otherwise.
(define (qmap? object)
  (hash-table? object))

;! Returns count of unique elements in a qmap.
;;
;; \param [in] qmap  A qmap.
;;
;; \returns non-negative exact integer.
(define (qmap-size qmap)
  (hash-table-size qmap))

;! Checks whether a qmap is empty.
;;
;; \param [in] qmap  A qmap.
;;
;; \returns \#t if #qmap is empty, \#f otherwise.
(define (qmap-empty? qmap)
  (= 0 (hash-table-size qmap)))

;! Checks whether a qmap contains a specified element.
;;
;; \param [in] qmap     A qmap.
;; \param [in] element  An arbitrary Scheme object.
;;
;; \returns \#t if #qmap contains #element, \#f otherwise.
(define (qmap-contains? qmap element)
  (hash-table-exists? qmap element))

;! Searches for an equal element in a qmap.
;;
;; \param [in] qmap     A qmap.
;; \param [in] element  An element to be searched for.
;; \param [in] default  Default value in case the element is absent.
;;
;; \returns an element of #qmap that is equal to #element,
;;          or #default if there are no such elements in #qmap
(define (qmap-member qmap element default)
  ; Meh, we have to manually look through the whole table to find what
  ; we need. Wish SRFI 69 hash tables provided an intrinsic for this.
  (let ((equal? (hash-table-equivalence-function qmap)))
    (qmap-for-each/stop qmap
      (lambda (found-element quantity stop)
        (when (equal? element found-element)
          (stop found-element)))
      (lambda () default))))

;! Returns element's quantity in a qmap.
;;
;; \param [in] qmap     A qmap.
;; \param [in] element  An element of interest.
;;
;; \returns nonnegative exact integer, 0 if #element is absent in #qmap.
(define (qmap-quantity qmap element)
  (hash-table-ref/default qmap element 0))

;! Checks whether two qmaps are disjoint.
;;
;; Qmaps are disjoint when they have no elements in common. Actual element
;; quantity does not matter. An empty qmap is disjoint with any other qmap,
;; including itself.
;;
;; It is an error to pass qmaps with different comparators or hash functions
;; to this procedure.
;;
;; \param [in] qmap1, qmap2  Qmaps to be checked. They must have equal
;;                           comparators and hash functions.
;;
;; \returns \#t if #qmap1 and #qmap2 are disjoint, \#f otherwise.
(define (qmap-disjoint? qmap1 qmap2)
  ; Look both ways
  (and (qmap-disjoint/half? qmap1 qmap2)
       (qmap-disjoint/half? qmap2 qmap1)))

(define (qmap-disjoint/half? qmap-haystack qmap-needle)
  (qmap-for-each/stop qmap-needle
    (lambda (needle quantity stop)
      (when (qmap-contains? qmap-haystack needle)
        (stop #f)))
    (lambda () #t)))

;! Increments quantity of an element in a qmap.
;;
;; It is okay to pass non-integer or non-exact quantites, they will be truncated
;; to exact integer values by force. It is okay to pass zero quantity, nothing
;; will be done in this case. It is okay to pass negative quantities, it will be
;; substracted from the current quantity instead of being added to it. However,
;; it is (probably) not okay to pass NaNs or infinities here.
;;
;; The element is removed from the qmap if its resuling quantity becomes zero
;; or less. If the element is absent in the qmap, it will be added (provided
;; that the resulting quantity is positive).
;;
;; \param [out] qmap      A qmap.
;; \param [in]  element   A (possible) element of #qmap to be modified.
;; \param [in]  quantity  Number of elements to be added to #qmap.
(define (qmap-inc! qmap element quantity)
  (qmap-set! qmap element (+ (qmap-quantity qmap element) quantity)))

;! Decrements quantity of an element in a qmap.
;;
;; It is okay to pass non-integer or non-exact quantites, they will be truncated
;; to exact integer values by force. It is okay to pass zero quantity, nothing
;; will be done in this case. It is okay to pass negative quantities, it will be
;; added to the current quantity instead of being substracted from it. However,
;; it is (probably) not okay to pass NaNs or infinities here.
;;
;; The element is removed from the qmap if its resuling quantity becomes zero
;; or less. If the element is absent in the qmap, it will be added (provided
;; that the resulting quantity is positive).
;;
;; \param [out] qmap      A qmap.
;; \param [in]  element   A (possible) element of #qmap to be modified.
;; \param [in]  quantity  Number of elements to be removed from #qmap.
(define (qmap-dec! qmap element quantity)
  (qmap-set! qmap element (- (qmap-quantity qmap element) quantity)))

;! Sets the quantity of an element in a qmap.
;;
;; It is okay to pass non-integer or non-exact quantites, they will be truncated
;; to exact integer values by force. However, it is (probably) not okay to pass
;; NaNs or infinities here.
;;
;; The element is removed from the qmap if its resuling quantity becomes zero
;; or less. If the element is absent in the qmap, it will be added (provided
;; that the resulting quantity is positive).
;;
;; \param [out] qmap      A qmap.
;; \param [in]  element   A (possible) element of #qmap to be modified.
;; \param [in]  quantity  The new number of elements in the #qmap.
(define (qmap-set! qmap element quantity)
  (let ((quantity (exact (truncate quantity))))
    (if (<= quantity 0)
        (hash-table-delete! qmap element)
        (hash-table-set! qmap element quantity))))

;! Removes an element from a qmap.
;;
;; Semantically equivalent to `(qmap-set! qmap element 0)`, but may be faster.
;; (It is okay to pass elements that are already absent in the qmap.)
;;
;; \param [out] qmap     A qmap.
;; \param [in]  element  An element to be removed.
(define (qmap-rem! qmap element)
  (hash-table-delete! qmap element))

;! Removes all elements from a qmap.
;;
;; \param [out] qmap  A qmap.
(define (qmap-clear! qmap)
  ; So much for the efficiency. Why, or why, SRFI 69?
  (for-each
    (lambda (element) (hash-table-delete! qmap element))
    (hash-table-keys qmap)))

;! Replaces an element value in a qmap.
;;
;; If #qmap contains an element that is equivalent to #element then that element
;; is replaced with #element (its quantity stays the same as it was). If there
;; is no such element in #qmap then nothing happens.
;;
;; \param [out] qmap     A qmap.
;; \param [in]  element  The new value of an element.
(define (qmap-replace! qmap element)
  (when (hash-table-exists? qmap element)
    (let ((quantity (hash-table-ref qmap element)))
      (hash-table-delete! qmap element)
      (hash-table-set! qmap element quantity))))

;! Searches a qmap for an element.
;;
;; #qmap is searched for #element. If it is found then #success is tail-called,
;; otherwise #failure is tail-called.
;;
;; #success is called with three arguments `(found-element update remove)`.
;; `found-element` is an element of #qmap that is equal to #element. `update`
;; and `remove` are continuations, #success is expected to tail-call either.
;;
;; #failure is called with two arguments `(insert ignore)`. `insert` and
;; `ignore` are also continuations, #failure is expected to tail-call either.
;;
;; Signatures and effects of the continuations are as follows:
;;
;;   * `(update new-element obj)` replaces a singled #element with `new-element`
;;
;;   * `(remove obj)` removes a single #element from #qmap
;;
;;   * `(insert obj)` inserts a single #element into #qmap
;;
;;   * `(ignore obj)` does nothing to #qmap
;;
;; `obj` can be an arbitrary Scheme value. It will become the value of the
;; whole call to #qmap-seach!
;;
;; It is an error to explicitly modify the qmap in #success or #failure
;; procedures. It can be modified only by tail-calling the respective
;; continuation. This restriction is imposed to allow #qmap-search! to
;; keep the search state around until the continuation is called.
;;
;; \param [in,out] qmap     A qmap.
;; \param [in]     element  An element to be searched for.
;; \param [in]     failure  A procedure accepting two arguments.
;; \param [in]     success  A procedure accepting three arguments.
;;
;; \returns the `obj` passed to either of the search continations.
(define (qmap-search! qmap element failure success)
  ; Not a pinncale of efficiency either. I imagine there could be some
  ; hash table instrinsic for this that avoids rehashing, etc.
  (define (update new-element obj)
    (qmap-dec! qmap element 1)
    (qmap-inc! qmap new-element 1)
    obj)

  (define (remove obj)
    (qmap-dec! qmap element 1)
    obj)

  (define (insert obj)
    (qmap-inc! qmap element 1)
    obj)

  (define (ignore obj)
    obj)

  (define not-found (cons 'not 'found))

  (let ((element (qmap-member qmap element not-found)))
    (if (eq? element not-found)
        (failure insert ignore)
        (success element update remove))))

;! Folds a whole qmap over its elements and their quantities.
;;
;; #proc is successively called with three arguments `(element quantity
;; value)`, where `element` is the next element of #qmap, `quantity` is its
;; quantity, and `value` is the value from the previous iteration. The value
;; returned by #proc will be the value of `value` in the next iteration. The
;; initial value of `value` is #value. The order of elements is unspecified.
;;
;; It is an error to modify the qmap while iterating over it.
;;
;; \param [in] qmap   A qmap.
;; \param [in] value  Initial value.
;; \param [in] proc   Folding procedure. `(element quantity value)`
;;
;; \returns the value returned by #proc in the last iteration,
;;          or #value if the qmap is empty.
(define (qmap-fold qmap value proc)
  (hash-table-fold qmap proc value))

;! Iterates over all qmap's element-quantity pairs.
;;
;; #proc is successively called with two arguments `(element quantity)`, where
;; `element` is the next element of #qmap, and `quantity` is its quantity. The
;; value returned by #proc is discarded. The order of elements is unspecified.
;;
;; It is an error to modify the qmap while iterating over it.
;;
;; \param [in] qmap   A qmap.
;; \param [in] proc   Iteration procedure. `(element quantity)`
(define (qmap-for-each qmap proc)
  (hash-table-walk qmap proc))

;! \fn qmap-for-each/stop
;;
;; \brief Iterates over qmap's element-quantity pairs.
;;
;; #proc is successively called with three arguments `(element quantity stop)`,
;; where `element` is the next element of #qmap, `quantity` is its quantity, and
;; `stop` is an escape procedure accepting one argument.
;;
;; The value returned by #proc is discarded. The order in which qmap's elements
;; are examined is unspecified.
;;
;; The iteration can be interrupted by invoking the `stop` escape procedure
;; from #proc (you are not required to tail-call it). In this case there will be
;; no more invocations of #proc, and #qmap-for-each/stop will immediately return
;; the value passed to `stop`.
;;
;; If the whole qmap had been iterated over without `stop` being called, the
;; #finally procedure is invoked without any arguments and its value becomes
;; the value of the #qmap-for-each/stop call.
;;
;; It is an error to modify the qmap while iterating over it.
;;
;; \param [in] qmap     A qmap.
;; \param [in] proc     Iteration procedure. `(element quantity stop)`
;; \param [in] finally  Terminal procedure. `()`
;;
;; \returns the value passed to `stop` or returned by #finally

(define-record-type StopIterationException
  (stop-iteration value)
  stop-iteration?
  (value stop-iteration-value))

; At least something could be done right!
(define (qmap-for-each/stop qmap proc finally)
  (guard (condition
          ((stop-iteration? condition) (stop-iteration-value condition)))
    (hash-table-walk qmap
      (lambda (element quantity)
        (proc element quantity
          (lambda (stop-value) (raise (stop-iteration stop-value))))))
    (finally)))
