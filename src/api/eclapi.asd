(defsystem "eclapi"
  :depends-on ("cffi")
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                (safety 3)
                                (speed 1)))
                    (funcall next))
  :components ((:file "eclapi")))
