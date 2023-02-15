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



(defun put-svg (data)
    (let ((svg "data:image/svg+xmlutf8,")
          (width (q< |width| ui:*main*))
      (height (q< |height| ui:*main*))
      (n 200)
      (m 10)
      (left-right-margin 24)
      (bottom-margin 14)
      (lbl-width 60))
      (handler-case
          (progn (set-input-parameter :width width :n n :height height :m m
                                      :left-right-margin left-right-margin
                                      :bottom-margin bottom-margin
                                      :lbl-width lbl-width)
                 (receive-data data)
                 (prepare-data)
                 (set-parameter)
                 (transform-data)
                 (setf svg (str+ svg (draw-svg :output :string))))
        (condition (c)
          (setf svg *svg*)
          (q> |svgText| ui:*rect3* (format nil "error '~a'" c))))
      (q> |svgText| ui:*rect3* svg))
  (values))
