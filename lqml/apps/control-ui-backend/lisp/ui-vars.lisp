(defpackage :ui
  (:use :cl :qml)
  (:export
   #:*main*
   #:*server*
   #:*header*
   #:frontpage
   #:*button*
   #:*rect3*
   #:*svg*
   #:*column*
   #:*wrect*
   #:*msgbox*
   #:*wsclient*))

(in-package :ui)

(defparameter *main* "main")
(defparameter *server* "server")
(defparameter *header* "header")
(defparameter *frontpage* "frontpage")
(defparameter *button* "btnWerkstattLicht")
(defparameter *rect3* "rctTempHum")
(defparameter *svg* "svg")
(defparameter *wrect* "rctMsgBox")
(defparameter *msgbox* "txtMsgBox")
(defparameter *wsclient* "wsclient")
