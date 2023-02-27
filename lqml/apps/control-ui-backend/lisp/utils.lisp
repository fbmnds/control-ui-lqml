(in-package :app)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))

(defun substring (s start &optional end)
  "Return the substring of `s' from `start' to `end'.
It uses `subseq' with differences:
- argument order, s at the end
- `start' and `end' can be lower than 0 or bigger than the length of s.
- for convenience `end' can be nil or t to denote the end of the string.
"
  (let* ((s-length (length s))
         (end (cond
                ((null end) s-length)
                ((eq end t) s-length)
                (t end))))
    (setf start (max 0 start))
    (if (> start s-length)
        ""
        (progn
          (setf end (min end s-length))
          (when (< end (- s-length))
            (setf end 0))
          (when (< end 0)
            (setf end (+ s-length end)))
          (if (< end start)
              ""
              (subseq s start end))))))

(defun b64-encode (object-name property text)
  (qml-set object-name property (base64:string-to-base64-string text))
  (values))

(defun b64-decode (qt-object property text64)
  (qml-set object-name property (base64:base64-string-to-string text64))
  (values))

(defun read-file (file)
  (with-open-file (s (merge-pathnames file (or *compile-file-truename*
                                               *load-truename*)))
    (let ((str (make-string (file-length s))))
      (read-sequence str s)
      str)))

