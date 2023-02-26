(in-package :app)

(ensure-permissions :internet)
(ensure-permissions :write-external-storage)
(ensure-permissions :access-network-state)


(defvar *svg-server* "http://192.168.178.8:7700/svg")
(defvar *werkstatt-licht* "http://192.168.178.11")

(defvar *svg2* (str+ "<svg version=\"1.1\" width=\"300\" height=\"200\""
                     " xmlns=\"http://www.w3.org/2000/svg\">"
                     "<rect width=\"100%\" height=\"100%\" fill=\"lavender\" />"
                     "<circle cx=\"150\" cy=\"100\" r=\"80\" fill=\"lightgrey\" />"
                     "<text x=\"150\" y=\"125\" font-size=\"60\""
                     " text-anchor=\"middle\" fill=\"green\">SVG</text></svg>"))
(defvar *svg* (str+ "data:image/svg+xml;utf8," *svg2*))

;;(defun img () (values *svg*))

(defun frontpage-state (state) (q> |state| ui:*frontpage* state))

(let ((current-status "{ \"r1\" : 0 }"))
  (defun werkstattlicht (slug)
    "Respond via broadcast iff status changed"
    (frontpage-state "WAIT")
    (qlog *werkstatt-licht* slug)
    (let ((status (ignore-errors
                   (string-trim '(#\r #\n) (curl (str+ *werkstatt-licht* slug))))))
      (cond ((null status)
             (frontpage-state "ERROR")
             (q> |wslStatus| ui:*wsth-svg* ""))
            ((search "{ \"r1\" : 1 }" status) (frontpage-state "ON"))
            ((search "{ \"r1\" : 0 }" status) (frontpage-state "OFF") )
            (t (qlog "app:werkstattlicht: unexpected status" status)))
      (when (string/= status current-status)
        (q! |addBroadcastEvent| ui:*wsth-svg* "/werkstattlicht" status)
        (setf current-status status)))
    (values)))

(defparameter *n-messages* 12)
(defparameter *messages* (make-array *n-messages*
                                     :element-type 'string
                                     :initial-element nil))

(defun lines ()
  (let ((s (aref *messages* 0)))
    (if (null s)
        (setf s "")
        (progn (loop for i from 1 to (1- *n-messages*)
                     do (x:when-it (aref *messages* i)
                                 (setf s (format nil "~a~%~a" s x:it))))))
    s))

(defun format-ht (ht)
  (when ht
    (format nil "~a | ~aC | ~a% | ~a"
            (gethash "ts" ht)
            (fmt10 (gethash "temp" ht))
            (fmt10 (gethash "hum" ht))
            (gethash "state" ht))))

(defun put-svg (data)
  (let ((svg "")
        (width (q< |width| ui:*wsth-svg*))
        (height (q< |height| ui:*wsth-svg*))
        (n 600)
        (m 10)
        (left-right-margin 24)
        (bottom-margin 14)
        (lbl-width 60)
        text)
    (handler-case
        (progn (set-input-parameter :width width :n n :height height :m m
                                    :left-right-margin left-right-margin
                                    :bottom-margin bottom-margin
                                    :lbl-width lbl-width)
               (receive-data data)
               (loop for i from 0 to (1- *n-messages*)
                     do (if (< i (length *data*))
                            (setf (aref *messages* i) (format-ht (nth i *data*)))
                            (setf (aref *messages* i) nil)))
               (prepare-data)
               (set-parameter)
               (transform-data)
               (setf svg (str+ svg (draw-svg :output :string)))
               (setf text (lines)))
      (condition (c)
        (setf svg *svg*)
        (setf text (format nil "error '~a'" c))
        (qlog (format nil "error '~a'" c))))
    (q> |svgText| ui:*wsth-svg* (str+ "data:image/svg+xml;utf8," svg))
    (q> |svgMsg| ui:*wsth-svg* text)
    (let ((svg-text-64 (base64:string-to-base64-string svg))
          (svg-msg-64 (base64:string-to-base64-string text)))
      (q> |svgText64| ui:*wsth-svg* svg-text-64)
      (q> |svgMsg64| ui:*wsth-svg* svg-msg-64)
      (qjs |addBroadcastEvent| ui:*wsth-svg* "/svg"
           (str+ "{ \"tag\": \"data\", \"text\": \""
                 svg-msg-64 "\", \"svg\": \""
                 svg-text-64 "\" }")))        
    (values)))

