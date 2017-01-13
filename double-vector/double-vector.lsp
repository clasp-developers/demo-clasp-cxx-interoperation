
;;;
;;; The path needs to be added to the load command
;;;
(setf *default-pathname-defaults* (pathname (concatenate 'string (ext:getenv "CLASP_DEMO_HOME") "/")))
(load "double-vector/doubleVector.bc")
(defparameter *a* (dv:make-double-vector-with-values '(1 2 3)))
(defparameter *b* (dv:make-double-vector-with-values '(4 5 6)))
(format t "dot product of (1 2 3).(4 5 6) --> ~a~%" (dv:dot *a* *b*))
(dv:fill *a* #(1.2 3.4 5.6))
(dv:dump *a* "gratuitous string!")
(dv:dump *a* )   ;; note: I left off the optional argument
(quit)
