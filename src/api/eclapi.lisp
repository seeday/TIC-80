(in-package :cl-user)

(defpackage #:t80
  (:use #:cl)
  (:export #:cls))

;; (ffi:load-foreign-library "tic80core" :system-library nil)

(ffi::clines "extern void ecl_api_cls(uint8_t color);")
(ffi::clines "extern void ecl_api_line(int32_t x1, int32_t y1, int32_t x2, int32_t y2, int8_t color);")

(ffi:def-function ("ecl_api_cls" cls) ((color :byte)) :returning :void)
(ffi:def-function ("ecl_api_line" line) ((x1 :int) (y1 :int) (x2 :int) (y2 :int) (color :byte)) :returning :void)

