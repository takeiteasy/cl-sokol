(defpackage #:cl-sokol
  (:nicknames :sokol)
  (:use #:cl #:cffi #:%sokol)
  (:export
   #:run
   #:init-cb
   #:frame-cb
   #:event-cb
   #:cleanup-cb))

(defpackage #:cl-sokol-test
  (:nicknames :sokol-test)
  (:use #:cl)
  (:export #:run))