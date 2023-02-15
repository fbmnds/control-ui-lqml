(ql:quickload '(:websocket-driver :websocket-driver-client))

(defpackage :test
  (:use :cl)
  (:local-nicknames ;;(#:lt #:local-time)
                    (#:ws #:websocket-driver)
                    (#:wsd #:websocket-driver-client)
                    ;;(#:bt #:bordeaux-threads)
                    ;;(#:lp #:lparallel)
                    ;;(#:lpq #:lparallel.queue)
                    )
  (:export #:send-svg))

(in-package :test)

(defmacro str+ (&rest rest) `(concatenate 'string ,@rest))

(defparameter *svg2* (str+ "<svg version=\"1.1\" width=\"300\" height=\"200\""
                     " xmlns=\"http://www.w3.org/2000/svg\">"
                     "<rect width=\"100%\" height=\"100%\" fill=\"lavender\" />"
                     "<circle cx=\"150\" cy=\"100\" r=\"80\" fill=\"lightgrey\" />"
                     "<text x=\"150\" y=\"125\" font-size=\"60\""
                     " text-anchor=\"middle\" fill=\"green\">SVG</text></svg>"))

(defparameter *fetch-db-cmd*
  (str+ "scp -i /home/dev/.ssh/bitbucket_rsa.pub"
        " dev@192.168.178.32:~/projects/heating-control/data/heating.db"
        " /home/dev/projects/heating-control/data/heating.db"))

(defun select-data (n)
  (str+
   "sqlite3 -json "
   " ~/projects/heating-control/data/heating.db"
   " 'select * from heating "
   " where not temp is null and not hum is null "
   " order by ts "
   (format nil " desc limit ~a;'" n)))

(defun fetch-data (n)
  (uiop:run-program *fetch-db-cmd*)
  (uiop:run-program (select-data n) :force-shell t
                    :output '(:string :stripped t)))

(defun send-svg(url n)
  (let* ((data (fetch-data n))
         (url (str+ "ws://" url ":7700/"))
         (client (wsd:make-client url)))
    (progn
      ;;(print data)
      ;;(ws:on :open client (lambda () (format t "~&connected~%")))
      (ws:start-connection client)
      (ws:on :message client (lambda (message) (format t "~a" message)))
      (ws:send client data)
      (sleep 1))
    (ws:close-connection client)))
