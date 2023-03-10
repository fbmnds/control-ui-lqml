(defpackage :ui
  (:use :cl :qml)
  (:export
   #:*main*
   #:*server*
   #:*socket*
   #:*tm-socket*
   #:*header*
   #:*frontpage*
   #:*frontpage-status*
   #:*wsl-button*
   #:*wsth-svg*
   #:*svg*
   #:*wsth-list*
   #:*wsth-list-text*
   ))

(in-package :ui)

(defparameter *main* "main")
(defparameter *server* "server")
(defparameter *socket* "socket")
(defparameter *tm-socket* "tmSocket")
(defparameter *header* "header")
(defparameter *frontpage* "frontpage")
(defparameter *frontpage-status* "frontpage_status")
(defparameter *wsl-button* "wsl_button")
(defparameter *wsth-svg* "wsth_svg")
(defparameter *svg* "svg")
(defparameter *wsth-list* "wsth_list")
(defparameter *wsth-list-text* "wsth_list_text")

