(in-package :hello-world)

(defun hello-world-from-lisp ()
  (format t "Hello World~%This is C++ code being invoked from Clasp Common Lisp~%"))

(defun demo ()
  (hello-world-from-c++)
  (hello-world-from-lisp)
  (format t "The result of (hw:add-three-numbers 1 2.0 3) --> ~a~%" (add-three-numbers 1 2.0 3)))

(export '(hello-world-from-lisp demo))
