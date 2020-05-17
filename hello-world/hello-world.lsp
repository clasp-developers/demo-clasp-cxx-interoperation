;;;
;;; The path to the bitcode file needs to be added to the load command argument
;;;

;;;(load (merge-pathnames #P"hello-world-cxx.bc" *load-truename*) )
(cmp:load-ir-run-c-function (merge-pathnames #P"hello-world-cxx.bc" *load-truename*)
                            "hello_world_startup")

(hw:hello-world)

(format t "The result of (hw:add-three-numbers 1 2.0 3) --> ~a~%" (hw:add-three-numbers 1 2.0 3))
(core:quit)
