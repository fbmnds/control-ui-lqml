
(defpackage :app
  (:use :cl :qml :cl-svg :parse-float)
  (:local-nicknames (#:lt #:local-time)
                    ;;(#:ws #:websocket-driver)
                    ;;(#:wsd #:websocket-driver-client)
                    ;;(#:bt #:bordeaux-threads)
                    ;;(#:lp #:lparallel)
                    ;;(#:lpq #:lparallel.queue)
                    )
  (:export    #:button-pressed
              #:put-svg
              #:werkstattlicht
              #:receive-data
              #:generate-svg
              ))


