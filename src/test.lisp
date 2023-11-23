;;;; test.lisp

(in-package #:sokol-test)

(defmethod sokol:init-cb ()
  (print "hello from init!"))

(defmethod sokol:frame-cb ()
  (print "hello from frame!"))

(defmethod sokol:event-cb (event)
  (print "hello from event!"))

(defmethod sokol:cleanup-cb ()
  (print "goodbye from cleanup!"))

(defun run ()
  (sokol:run 640 480 "test!!"))
