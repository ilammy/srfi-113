(package
  (name (srfi 113))
  (version "1.0.0")
  (description "SRFI 113 'Comparators'")
  (homepage "https://github.com/ilammy/srfi-113")
  (authors "ilammy")
  (depends
    (scheme base)
    (scheme case-lambda)
    (srfi 60)
    (srfi 69)
    (srfi 114)
    (srfi 114 default-comparator))
  (library
    (name (srfi 113))
    (path "srfi/113.sld")
    (test-program "test/srfi-113-tests.scm"))
  (program
    (use-for test)
    (path "test/srfi-113-tests.scm")
    (depends
      (chibi)
      (chibi test)
      (srfi 1)
      (srfi 113)
      (srfi 114))))
