(defpackage :app
  (:use :cl :qml :cl-svg :parse-float :cl-base64)
  (:local-nicknames (#:lt #:local-time))
  (:export #:button-pressed
           #:werkstattlicht
           #:receive-data
           #:generate-svg
           #:put-svg
           #:b64-encode
           #:b64-decode
           #:websocket-server-connect
           #:websocket-client-connect))


