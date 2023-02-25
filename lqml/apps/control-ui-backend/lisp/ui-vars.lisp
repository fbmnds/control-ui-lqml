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
(defparameter *wsl-button* "wsl_button")
(defparameter *wsth-svg* "wsth_svg")
(defparameter *svg* "svg")
(defparameter *wrect* "rctMsgBox")
(defparameter *msgbox* "txtMsgBox")

