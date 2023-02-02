(in-package :app)


(defun update-status (text color)
  (qjs |set| ui:*button* color)
  (q> |text| ui:*button* text))

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
  (qsleep 1)
  (ignore-errors (set-status (curl "http://192.168.178.11/r1")))
  (values))                               

(defun werkstattlicht ()
  (update-status "Werkstattlicht ..." "lightyellow")
  (qsleep 1)
  (ignore-errors (set-status (curl "http://192.168.178.11/?")))
  (values))
