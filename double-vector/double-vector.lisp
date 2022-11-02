(in-package #:double-vector)

(defun demo ()
  (let ((a (dv:make-double-vector-with-values '(1 2 3)))
        (b (dv:make-double-vector-with-values '(4 5 6))))
    (format t "dot product of (1 2 3).(4 5 6) --> ~a~%" (dv:dot a b))
    (dv:fill a #(1.2 3.4 5.6))
    (dv:dump a "gratuitous string!")
    (dv:dump a)))   ;; note: I left off the optional argument

(export 'demo)
