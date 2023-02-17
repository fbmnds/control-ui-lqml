(defpackage :ui
  (:use :cl :qml)
  (:export
   #:*main*
   #:*label*
   #:*button*
   #:*rect3*
   #:*svg*
   #:*column*
   #:*wrect*
   #:*msgbox*))

(in-package :ui)

(defparameter *main* "main")
(defparameter *label* "label")
(defparameter *column* "column")
(defparameter *button* "btnWerkstattLicht")
(defparameter *rect3* "rctTempHum")
(defparameter *svg* "svg")
(defparameter *wrect* "rctMsgBox")
(defparameter *msgbox* "txtMsgBox")
