;;;; attributes-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage git-api.test.attributes-test
  (:use :cl
        :git-api.utils
        :git-api.test.base
        :git-api.plumbing.helpers.details.attributes
        :prove))
(in-package :git-api.test.attributes-test)


(from git-api.plumbing.helpers.details.attributes import
      parse-collected-attributes)

(plan nil)

(subtest "Testing parse-collected-attributes"
  (is (parse-collected-attributes '("-diff" "-merge" "-text"))
      '(("diff" . :unset) ("merge" . :unset) ("text" . :unset))
      :test #'equalp
      "Testing input: ('-diff' '-merge' '-text')"))

(subtest "Testing parse-attribute-line"
  (is (parse-attribute-line "a*      foo !bar -baz")
      (cons "a*" '(("foo" . :set) ("bar" . :unspecified) ("baz" . :unset)))
      :test #'equalp
      "Testing input: 'a*      foo !bar -baz'")
  
  (is (parse-attribute-line "abc     foo bar baz")
      (cons "abc" '(("foo" . :set) ("bar" . :set) ("baz" . :set)))
      :test #'equalp
      "Testing input: 'abc     foo bar baz'")
  
  (is (parse-attribute-line "ab*     merge=filfre")
      (cons "ab*" '(("merge" . "filfre")))
      :test #'equalp
      "Testing input: 'ab*     merge=filfre'")
  
  (is (parse-attribute-line "abc     -foo -bar")
      (cons "abc" '(("foo" . :unset) ("bar" . :unset)))
      :test #'equalp
      "Testing input: 'abc     -foo -bar'")
  
  (is (parse-attribute-line "*.c     frotz")
      (cons "*.c" '(("frotz" . :set)))
      :test #'equalp
      "Testing input: '*.c     frotz"))


(subtest "Test parse-attribute-macro"
  (is (parse-attribute-macro "[attr]binary -diff -merge -text")
      '("binary" ("diff" . :unset) ("merge" . :unset) ("text" . :unset))
      "Testing input: '[attr]binary -diff -merge -text'"))


(finalize)
