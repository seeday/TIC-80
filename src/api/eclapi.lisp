(in-package :cl-user)

(defpackage #:t80
  (:use #:cl)
  (:export #:cls))

;; (ffi:load-foreign-library "tic80core" :system-library nil)

 (ffi::clines "extern void eclApiCls(int color);")
 (ffi:def-function ("eclApiCls" cls) ((arg :byte)) :returning :void)
 ;; (defun cls (color)
 ;; (ffi::clines "extern void eclApiCls(int color);")
 ;;  (ffi::c-inline (color) (:uint8_t) :void "eclApiCls(#0);" :one-liner t)
 ;;  )
