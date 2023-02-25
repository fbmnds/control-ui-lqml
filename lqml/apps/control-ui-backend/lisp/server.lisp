(in-package :app)

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

(defun websocket-server-connect (src socket)
  (qlog "websocket-server-connect " (substring src 0 20)
        " " (q< |objectName| *caller*)
        " " (q< |objectName| ui:*server*)
        " " (format nil "~a" (q< |url| socket))
        " " (format nil "~a" (q< |url| ui:*server*)))
  (let ((server ui:*server*)
        (ws-url (format nil "~a" (q< |url| socket))))
    (q! |appendMessage| ui:*wrect*
        (str+ "for url: " ws-url " " (substring src 0 20)))
    (cond ((x:ends-with "/werkstattlicht/" ws-url) ; '?' omitted in socket.url
           (let ((wsl-status (q< |wslStatus| ui:*rect3*)))
             (qlog (str+ "return on /werkstattlicht/? " wsl-status))
             (q! |sendTextMessage| server wsl-status)))
          ((x:ends-with "/werkstattlicht/r1" ws-url)
           (werkstattlicht "/r1"))
          ((x:starts-with "<?xml" src)
           (q> |source| ui:*svg* (str+ "data:image/svg+xml;utf8," src)))
          ((x:starts-with "data:image/svg+xml;utf8," src)
           (q> |source| ui:*svg* src))
          ((x:starts-with "[" src)
           (put-svg src)
           (q! |setMessage| ui:*wrect* (q< |svgMsg| ui:*rect3*)))
          (t (q! |setMessage| ui:*wrect* (str+ "ignoring " (substring src 0 20)
                                               " for url " ws-url)))))
  (values))
