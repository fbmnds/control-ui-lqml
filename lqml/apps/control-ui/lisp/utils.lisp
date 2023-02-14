(in-package :app)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))

(defun read-file (file)
  (with-open-file (s (merge-pathnames file (or *compile-file-truename*
                                               *load-truename*)))
    (let ((str (make-string (file-length s))))
      (read-sequence str s)
      str)))

