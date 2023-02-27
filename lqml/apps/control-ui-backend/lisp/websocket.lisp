(in-package :app)

(defun websocket-server-connect (src socket)
  (qlog "websocket-server-connect " (substring src 0 20)
        " " (q< |objectName| *caller*)
        " " (q< |objectName| ui:*server*)
        " " (format nil "~a" (q< |url| socket))
        " " (format nil "~a" (q< |url| ui:*server*)))
  (let (;;(server ui:*server*)
        (ws-url (format nil "~a" (q< |url| socket))))
    (q! |appendMessage| ui:*wsth-list*
        (str+ "for url: " ws-url " " (substring src 0 20)))
    (cond ((x:ends-with "/werkstattlicht/" ws-url) ; '?' omitted in socket.url
           (let ((wsl-status (q< |msg| ui:*socket*)))
             (qlog (str+ "return on /werkstattlicht/? " wsl-status))
             (q! |sendTextMessage| socket wsl-status)))
          ((x:ends-with "/werkstattlicht/r1" ws-url)
           (werkstattlicht "/r1"))
          ((x:starts-with "<?xml" src)
           (q> |source| ui:*svg* (str+ "data:image/svg+xml;utf8," src)))
          ((x:starts-with "data:image/svg+xml;utf8," src)
           (q> |source| ui:*svg* src))
          ((x:starts-with "[" src)
           (put-svg src)
           (q! |setMessage| ui:*wsth-list* (q< |svgMsg| ui:*wsth-svg*)))
          (t (q! |setMessage| ui:*wsth-list* (str+ "ignoring " (substring src 0 20)
                                               " for url " ws-url)))))
  (values))

(defun broadcast-next-client (socket)
  (q> |active| socket nil)
  (q> |timer.running| socket nil)
  (q! |broadcast| ui:*wsth-svg*))

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
           (q! |appendMessage| ui:*wsth-list*
               (str+ "Error: " url " " (q< |errorString| socket)))
           (broadcast-next-client socket))
          ;;
          ((q< |open| socket)
           (qlog "socket open, sending..." (substring (q< |msg| socket) 0 20))
           (q! |sendTextMessage| socket (q< |msg| socket))
           ;;(broadcast-next-client socket)
           )
          ;;
          ((q< |closing| socket))
          ;;
          ((q< |closed| socket)
           (qlog "socket closed for " url)
           (broadcast-next-client socket)
           ;; broadcast-next-client triggered in either |open| or |error|
           )
          ;; should never happen:
          (t (qlog "unknown websocket status '"
                   (q< |status| socket) "' for url" url)
             (broadcast-next-client socket))))
  (values))
