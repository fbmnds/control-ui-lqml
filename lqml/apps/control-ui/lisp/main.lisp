(in-package :app)

(ensure-permissions :internet)
(ensure-permissions :write-external-storage)
(ensure-permissions :access-network-state)

(defvar *svg-server* "http://192.168.178.8:7700/svg")
(defvar *werkstatt-licht* "http://192.168.178.11")

(defvar *svg2* (str+ "data:image/svg+xml;utf8,"
                     "<svg version=\"1.1\" width=\"300\" height=\"200\""
                     " xmlns=\"http://www.w3.org/2000/svg\">"
                     "<rect width=\"100%\" height=\"100%\" fill=\"lavender\" />"
                     "<circle cx=\"150\" cy=\"100\" r=\"80\" fill=\"lightgrey\" />"
                     "<text x=\"150\" y=\"125\" font-size=\"60\""
                     " text-anchor=\"middle\" fill=\"green\">SVG</text></svg>"))
(defvar *svg* *svg2*)

(defun img ()
  (values *svg*))

(defun update-status (text color)
  (q> |background.color| ui:*button* color)
  (q> |text| ui:*button* text)
  (qsleep 1))

(defun set-status (status)
  (qlog (format nil "status ~A ~A" (local-time:now) status))
  (cond ((search "{ \"r1\" : 1 }" status)
         (update-status "Werkstattlicht AN" "lightgreen"))
        ((search "{ \"r1\" : 0 }" status)
         (update-status "Werkstattlicht AUS" "lightgrey"))
        (t
         (qlog (str+ "in set-status " status))
         (update-status "Werkstattlicht ..." "red"))))

(defun button-pressed ()
  (update-status "Werkstattlicht ..." "lightyellow")
  (qjs |wslRequest| ui:*wsclient* "/r1")
  (values))                               

(defun werkstattlicht (status)  
  (update-status "Werkstattlicht ..." "lightyellow")
  (set-status status)
  (values))

(defun %b64-decode (var s64)
  (let ((symb (make-symbol var)))
    (q> symb ui:*rect3* (base64:base64-string-to-string s64)))
  (values))

(defun b64-decode (var s64)
  (when (equal var "svgText64")
    (q> |svgText64| ui:*rect3* (base64:base64-string-to-string s64)))
  (when (equal var "svgMsg64")
    (q> |svgMsg64| ui:*rect3* (base64:base64-string-to-string s64)))
  (values))
