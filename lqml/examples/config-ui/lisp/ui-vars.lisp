(defpackage ui
  (:use :cl :qml)
  (:export
   #:*main*
   #:*label*
   #:*button*))

(in-package :ui)

(defparameter *main* "main")
(defparameter *label* "label")
(defparameter *button* "button")
