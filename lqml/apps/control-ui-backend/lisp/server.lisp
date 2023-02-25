(in-package :app)

(defun websocket-server-connect (src socket)
  (qlog "websocket-server-connect " (substring src 0 20)
        " " (q< |objectName| *caller*)
        " " (q< |objectName| ui:*server*)
        " " (format nil "~a" (q< |url| socket))
        " " (format nil "~a" (q< |url| ui:*server*)))
  (let ((server ui:*server*)
        (ws-url (format nil "~a" (q< |url| socket))))
    (q! |appendMessage| ui:*wrect*
        (str+ "for url: " ws-url " " (substring src 0 20)))
    (cond ((x:ends-with "/werkstattlicht/" ws-url) ; '?' omitted in socket.url
           (let ((wsl-status (q< |wslStatus| ui:*rect3*)))
             (qlog (str+ "return on /werkstattlicht/? " wsl-status))
             (q! |sendTextMessage| server wsl-status)))
          ((x:ends-with "/werkstattlicht/r1" ws-url)
           (werkstattlicht "/r1"))
          ((x:starts-with "<?xml" src)
           (q> |source| ui:*svg* (str+ "data:image/svg+xml;utf8," src)))
          ((x:starts-with "data:image/svg+xml;utf8," src)
           (q> |source| ui:*svg* src))
          ((x:starts-with "[" src)
           (put-svg src)
           (q! |setMessage| ui:*wrect* (q< |svgMsg| ui:*rect3*)))
          (t (q! |setMessage| ui:*wrect* (str+ "ignoring " (substring src 0 20)
                                               " for url " ws-url)))))
  (values))

(defun websocket-client-connect ()
  (qlog "websocket-client-connect"
        (q< |status| ui:*socket*)
        (q< |connecting| ui:*socket*)
        (q< |open| ui:*socket*)
        (q< |closed| ui:*socket*))
  (let* ((socket ui:*socket*)
         (url (q< |url| socket)))
    (cond ((q< |connecting| socket)
           (qlog "send to url" url)
           (q> |running| ui:*tm-socket* t)) ; set timeout for current connection
          ;;
          ((q< |error| socket)
           (q! |appendMessage| ui:*wrect*
               (str+ "Error: " url " " (q< |errorString| socket)))
           (q> |active| socket nil))
          ;;
          ((q< |open| socket)
           (qlog "Socket open, sending...")
           (q! |sendTextMessage| socket (q< |wsmsg| ui:*rect3*))
           (when (x:ends-with "/svg" url) (q> |active| socket nil)))
          ;;
          ((q< |closing| socket))
          ;;
          ((q< |closed| socket)
           (qlog "socket closed for " url)
           (q> |active| socket nil))
          ;;
          (t (qlog "unknown websocket status '"
                   (q< |status| socket) "' for url" url)
             (q> |active| socket nil))))
  (values))
