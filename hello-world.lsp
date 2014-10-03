(core:load-bundle "helloWorld.bundle")
(setq a (hw:make-double-vector-with-values '(1 2 3)))
(setq b (hw:make-double-vector-with-values '(4 5 6)))
(format t "The dot product is ~a~%" (hw:dot a b))
