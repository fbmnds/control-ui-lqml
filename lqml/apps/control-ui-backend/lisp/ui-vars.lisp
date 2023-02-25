(defpackage :ui
  (:use :cl :qml)
  (:export
   #:*main*
   #:*server*
   #:*socket*
   #:*tm-socket*
   #:*header*
   #:*frontpage*
   #:*wsl-button*
   #:*wsth-svg*
   #:*svg*
   #:*wsth-list*
   #:*msgbox*
   ))

(in-package :ui)

(defparameter *main* "main")
(defparameter *server* "server")
(defparameter *socket* "socket")
(defparameter *tm-socket* "tmSocket")
(defparameter *header* "header")
(defparameter *frontpage* "frontpage")
(defparameter *wsl-button* "wsl_button")
(defparameter *wsth-svg* "wsth_svg")
(defparameter *svg* "svg")
(defparameter *wsth-list* "wsth_list")
(defparameter *msgbox* "txtMsgBox")

