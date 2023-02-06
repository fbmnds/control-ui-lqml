(defsystem :app
  :serial t
  :depends-on (:local-time :yason)
  :components ((:file "lisp/package")
               (:file "lisp/curl")
               (:file "lisp/utils")
               (:file "lisp/ui-vars")
               (:file "lisp/main")))

