(defsystem :app
  :serial t
  :depends-on (:ecl-curl :local-time :yason :cl-svg :parse-float :cl-base64)
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/curl")
               (:file "lisp/utils")
               (:file "lisp/x")
               (:file "lisp/svg-lib")
               (:file "lisp/websocket")
               (:file "lisp/main")))

