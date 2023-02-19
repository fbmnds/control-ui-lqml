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

(defun img ()
  (values *svg*))

(defun update-status (text color)
  (qjs |set| ui:*button* color)
  (q> |text| ui:*button* text)
  (qsleep 1))

(defun set-status (status)
  (qlog (format nil "status ~A ~A" (local-time:now) status))
  (cond ((search "{ \"r1\" : 1 }" status)
         (update-status "Werkstattlicht AN" "lightgreen"))
        ((search "{ \"r1\" : 0 }" status)
         (update-status "Werkstattlicht AUS" "lightgrey"))
        (t
         (update-status "Werkstattlicht ..." "lightred"))))

(defun button-pressed ()
  (update-status "Werkstattlicht ..." "lightyellow")
  (ignore-errors (set-status (curl (str+ *werkstatt-licht* "/r1"))))
  (values))

(defun werkstattlicht ()
  (update-status "Werkstattlicht ..." "lightyellow")
  (ignore-errors (set-status (curl (str+ *werkstatt-licht* "/?"))))
  (values))

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
        (width (q< |width| ui:*rect3*))
        (height (q< |height| ui:*rect3*))
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
    (q> |svgText| ui:*rect3* (str+ "data:image/svg+xml;utf8," svg))
    (q> |svgMsg| ui:*rect3* text)
    (q> |svgText64| ui:*rect3* (base64:string-to-base64-string svg))
    (values)))


(defun b64-decode (s64)
  (q> |svgText64| ui:*rect3* (base64:base64-string-to-string s64))
  (values))
