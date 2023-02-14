(defsystem :app
  :serial t
  :depends-on (:ecl-curl :local-time :yason)
  :components ((:file "lisp/package")
               (:file "lisp/curl")
               (:file "lisp/utils")
               (:file "lisp/ui-vars")
               (:file "lisp/main")))

