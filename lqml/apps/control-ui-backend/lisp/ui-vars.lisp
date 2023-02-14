(defpackage ui
  (:use :cl :qml)
  (:export
   #:*main*
   #:*label*
   #:*button*
   #:*rect3*
   #:*svg*
   #:*column*
   #:*wrect*
   #:*wrect3*))

(in-package :ui)

(defparameter *main* "main")
(defparameter *label* "label")
(defparameter *button* "button")
(defparameter *rect3* "rect3")
(defparameter *column* "column")
(defparameter *wrect* "wrect")
(defparameter *wrect3* "wrect3")
