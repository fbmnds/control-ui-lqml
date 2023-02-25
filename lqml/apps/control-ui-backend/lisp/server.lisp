(in-package :app)

(defun substring (start end s)
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

(defun starts-with-p (start s &key ignore-case)
  "Return t if S starts with the substring `START', nil otherwise.
  START can be a string or a character."
  (let ((start-length (length (string start))))
    (when (>= (length s) start-length)
      (let ((fn (if ignore-case #'string-equal #'string=)))
        (funcall fn s start :start1 0 :end1 start-length)))))

(defun ends-with-p (end s &key ignore-case)
  "Return t if s ends with the substring 'end', nil otherwise.
  END can be a character or a string."
  (let ((s-length (length s))
        (end-length (length (string end))))
    (when (>= s-length end-length)
      (let ((fn (if ignore-case #'string-equal #'string=)))
        (funcall fn s end :start1 (- s-length end-length))))))

(defun websocket-server-connect (src socket)
  (qlog "websocket-server-connect " (substring 0 20 src)
        " " (q< |objectName| *caller*)
        " " (q< |objectName| ui:*server*)
        " " (format nil "~a" (q< |url| socket))
        " " (format nil "~a" (q< |url| ui:*server*)))
  (let ((server ui:*server*)
        (ws-url (format nil "~a" (q< |url| socket))))
    (q! |appendMessage| ui:*wrect*
        (str+ "for url: " ws-url " " (substring 0 20 src)))
    (cond ((ends-with-p "/werkstattlicht/" ws-url) ; '?' omitted in socket.url
           (let ((wsl-status (q< |wslStatus| ui:*rect3*)))
             (qlog (str+ "return on /werkstattlicht/? " wsl-status))
             (q! |sendTextMessage| server wsl-status)))
          ((ends-with-p "/werkstattlicht/r1" ws-url)
           (werkstattlicht "/r1"))
          ((starts-with-p "<?xml" src)
           (q> |source| ui:*svg* (str+ "data:image/svg+xml;utf8," src)))
          ((starts-with-p "data:image/svg+xml;utf8," src)
           (q> |source| ui:*svg* src))
          ((starts-with-p "[" src)
           (put-svg src)
           (q! |setMessage| ui:*wrect* (q< |svgMsg| ui:*rect3*)))
          (t (q! |setMessage| ui:*wrect* (str+ "ignoring " (substring 0 20 src)
                                               " for url " ws-url)))))
  (values))
