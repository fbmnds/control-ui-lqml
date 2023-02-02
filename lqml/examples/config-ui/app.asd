(defsystem :app
  :serial t
  :depends-on (:local-time)
  :components ((:file "lisp/package")
               (:file "lisp/curl")
               (:file "lisp/utils")
               (:file "lisp/ui-vars")
               (:file "lisp/main")))

