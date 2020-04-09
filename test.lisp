(in-package :rephrase)

(defun test ()
  (scanner (asdf:system-relative-pathname :rephrase2 "test.dsl")))

