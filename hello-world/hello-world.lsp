;;;
;;; The path to the bitcode file needs to be added to the load command argument
;;;

(setf *default-pathname-defaults* (pathname (concatenate 'string (ext:getenv "CLASP_DEMO_HOME") "/")))
(load "hello-world/helloWorld.bc")
(hw:hello-world)

(format t "The result of (hw:add-three-numbers 1 2.0 3) --> ~a~%" (hw:add-three-numbers 1 2.0 3))
(quit)
