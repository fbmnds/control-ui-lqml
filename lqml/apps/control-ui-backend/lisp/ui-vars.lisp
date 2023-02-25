(defpackage :ui
  (:use :cl :qml)
  (:export
   #:*main*
   #:*server*
   #:*socket*
   #:*tm-socket*
   #:*header*
   #:*frontpage*
   #:*button*
   #:*rect3*
   #:*svg*
   #:*wrect*
   #:*msgbox*
   ))

(in-package :ui)

(defparameter *main* "main")
(defparameter *server* "server")
(defparameter *socket* "socket")
(defparameter *tm-socket* "tmSocket")
(defparameter *header* "header")
(defparameter *frontpage* "frontpage")
(defparameter *button* "btnWerkstattLicht")
(defparameter *rect3* "rctTempHum")
(defparameter *svg* "svg")
(defparameter *wrect* "rctMsgBox")
(defparameter *msgbox* "txtMsgBox")

