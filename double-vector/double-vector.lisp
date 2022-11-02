(in-package #:double-vector)

(defmethod print-object ((object double-vector) stream)
  (print-unreadable-object (object stream :type t)
    (loop for i below (dimension object)
          unless (zerop i)
            do (write-char #\Space stream)
          do (write (vref object i) :stream stream)))
  object)

(defun (setf vref) (new-value object index)
  (setf-vref object new-value index))

(defun demo ()
  (let ((a (double-vector 1 2 3))
        (b (double-vector 4 5 6)))
    (format t "Dot product of ~s . ~s = ~a~%" a b (dot a b))
    (format t "Vector sum of ~s + ~s = ~a~%" a b (add a b))))

(export 'demo)
