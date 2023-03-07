(in-package :app)

(ensure-permissions :internet)
(ensure-permissions :write-external-storage)
(ensure-permissions :access-network-state)


(defun update-status (text color)
  (q> |background.color| ui:*button* color)
  (q> |text| ui:*button* text)
  (qsleep 1))

(defun button-pressed ()
  (update-status "Werkstattlicht ..." "lightyellow")
  (values))                               

