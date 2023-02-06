(in-package :app)

(defvar *svg-server* "http://192.168.178.8:7700/svg")
(defvar *werkstatt-licht* "http://192.168.178.11/?")

(defvar *svg* "data:image/svg+xml;utf8,<svg version=\"1.1\" width=\"300\" height=\"200\" xmlns=\"http://www.w3.org/2000/svg\"><rect width=\"100%\" height=\"100%\" fill=\"lavender\" /><circle cx=\"150\" cy=\"100\" r=\"80\" fill=\"lightgrey\" /><text x=\"150\" y=\"125\" font-size=\"60\" text-anchor=\"middle\" fill=\"red\">SVG</text></svg>")

(defun img ()
  (values *svg*))

(defun update-status (text color)
  (qjs |set| ui:*button* color (img))
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
  (ignore-errors (set-status (curl "http://192.168.178.11/r1")))
  (values))                               

(defun set-svg ()
  (let ((svg (ignore-errors (curl *svg-server*))))
    (when svg (setf *svg* (format nil "data:image/svg+xml;utf8,~a" svg)))))

(defun werkstattlicht ()  
  (update-status "Werkstattlicht ..." "lightyellow")
  (ignore-errors (set-status (curl *werkstatt-licht*)))
  (set-svg)
  (values))
