(defpackage ui
  (:use :cl :qml)
  (:export
   #:*main*
   #:*label*
   #:*button*
   #:*rect3*
   #:*svg*))

(in-package :ui)

(defparameter *main* "main")
(defparameter *label* "label")
(defparameter *button* "button")
(defparameter *rect3* "rect3")
(defparameter *column* "column")
