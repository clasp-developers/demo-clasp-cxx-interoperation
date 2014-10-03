(core:load-bundle "helloWorld.bundle")
(hw:hello-world)

(format t "The result of (hw:add-three-numbers 1 2.0 3) --> ~a~%" (hw:add-three-numbers 1 2.0 3))
(quit)
