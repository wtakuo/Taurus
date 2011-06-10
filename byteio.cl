;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; byteio.cl
;;;
;;; Copyright (C) 2003 Takuo Watanabe. All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above
;;;    copyright notice, this list of conditions and the following
;;;    disclaimer in the documentation and/or other materials provided
;;;    with the distribution.
;;; 3. The name of the author may not be used to endorse or promote
;;;    products derived from this software without specific prior written
;;;    permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :taurus)

(defun read-uint2 (stream
		   &optional (eof-error-p t) (eof-value nil))
  (let* ((b0 (read-byte stream nil -1))
	 (b1 (read-byte stream nil -1)))
    (if (= b1 -1)
	(if eof-error-p (error "EOF") eof-value)
      (bytes-to-uint2 b0 b1))))

(defun read-int2 (stream
		  &optional (eof-error-p t) (eof-value nil))
  (uint2-to-int2
   (read-uint2 stream eof-error-p eof-value)))

(defun read-uint4 (stream
		   &optional (eof-error-p t) (eof-value nil))
  (let* ((b0 (read-byte stream nil -1))
	 (b1 (read-byte stream nil -1))
	 (b2 (read-byte stream nil -1))
	 (b3 (read-byte stream nil -1)))
    (if (= b3 -1)
	(if eof-error-p (error "EOF") eof-value)
      (bytes-to-uint4 b0 b1 b2 b3))))

(defun read-int4 (stream
		  &optional (eof-error-p t) (eof-value nil))
  (uint4-to-int4
   (read-uint4 stream eof-error-p eof-value)))

(defun read-uint8 (stream
		   &optional (eof-error-p t) (eof-value nil))
  (let ((x 0))
    (dotimes (i 8)
      (let ((b (read-byte stream nil -1)))
	(if (= b -1)
	    (if eof-error-p
		(error "EOF")
	      (return-from read-uint8 eof-value)))
	(setq x (+ (* x 256) b))))
    x))

(defun read-int8 (stream
		  &optional (eof-error-p t) (eof-value nil))
  (uint8-to-int8
   (read-uint8 stream eof-error-p eof-value)))

(defun write-uint2 (x stream)
  (multiple-value-bind (b0 b1) (uint2-to-bytes x)
    (write-byte b0 stream)
    (write-byte b1 stream)))

(defun write-int2 (x stream)
  (write-uint2 (int2-to-uint2 x) stream))

(defun write-uint4 (x stream)
  (multiple-value-bind (b0 b1 b2 b3) (uint4-to-bytes x)
    (write-byte b0 stream)
    (write-byte b1 stream)
    (write-byte b2 stream)
    (write-byte b3 stream)))

(defun write-int4 (x stream)
  (write-uint4 (int4-to-uint4 x) stream))

(defun write-uint8 (x stream)
  (multiple-value-bind (b0 b1 b2 b3 b4 b5 b6 b7) (uint8-to-bytes x)
    (write-byte b0 stream)
    (write-byte b1 stream)
    (write-byte b2 stream)
    (write-byte b3 stream)
    (write-byte b4 stream)
    (write-byte b5 stream)
    (write-byte b6 stream)
    (write-byte b7 stream)))

(defun write-int8 (x stream)
  (write-uint8 (int8-to-uint8 x) stream))

(defun bytes-to-uint2 (b0 b1) 
  (+ (* b0 256) b1))

(defun bytes-to-uint4 (b0 b1 b2 b3)
  (+ (* 256 (+ (* 256 (+ (* 256 b0) b1)) b2)) b3))

(defun int2-to-uint2 (x)
  (if (< x 0) (+ x 65536) x))

(defun uint2-to-int2 (x)
  (if (> x 32767) (- x 65536) x))

(defun int4-to-uint4 (x)
  (if (< x 0) (+ x 4294967296) x))

(defun uint4-to-int4 (x)
  (if (> x 2147483647) (- x 4294967296) x))

(defun int8-to-uint8 (x) 
  (if (< x 0) (+ x 18446744073709551616) x))
  
(defun uint8-to-int8 (x)
  (if (> x 9223372036854775807) (- x 18446744073709551616) x))

(defun uint2-to-byte-list (x)
  (multiple-value-list (uint2-to-bytes x)))

(defun uint4-to-byte-list (x)
  (multiple-value-list (uint4-to-bytes x)))

(defun uint2-to-bytes (x)
  (values (mod (ash x -8) 256)
	  (mod x 256)))

(defun uint4-to-bytes (x)
  (values (mod (ash x -24) 256)
	  (mod (ash x -16) 256)
	  (mod (ash x -8) 256)
	  (mod x 256)))

(defun uint8-to-bytes (x)
  (values (mod (ash x -56) 256)
	  (mod (ash x -48) 256)
	  (mod (ash x -40) 256)
	  (mod (ash x -32) 256)
	  (mod (ash x -24) 256)
	  (mod (ash x -16) 256)
	  (mod (ash x -8) 256)
	  (mod x 256)))


;;; IEEE754 Floating-point numbers

(defun read-float (stream
		   &optional (eof-error-p t) (eof-value nil))
  #+EXCL
  (let* ((s0 (read-uint2 stream nil -1))
	 (s1 (read-uint2 stream nil -1)))
    (if (= s1 -1)
	(if eof-error-p (error "EOF") eof-value)
      (shorts-to-single-float s0 s1)))
  #-EXCL
  (read-byte-array 4 stream eof-error-p eof-value))

(defun write-float (x stream)
  #+EXCL
  (multiple-value-bind (s0 s1) (single-float-to-shorts x)
    (write-uint2 s0 stream)
    (write-uint2 s1 stream))
  #-EXCL
  (write-byte-array x stream))

(defun read-double (stream
		    &optional (eof-error-p t) (eof-value nil))
  #+EXCL
  (let* ((s0 (read-uint2 stream nil -1))
	 (s1 (read-uint2 stream nil -1))
	 (s2 (read-uint2 stream nil -1))
  	 (s3 (read-uint2 stream nil -1)))
    (if (= s3 -1)
	(if eof-error-p (error "EOF") eof-value)
      (shorts-to-double-float s0 s1 s2 s3)))
  #-EXCL
  (read-byte-array 8 stream eof-error-p eof-value))

(defun write-double (x stream)
  #+EXCL
  (multiple-value-bind (s0 s1 s2 s3) (double-float-to-shorts x)
    (write-uint2 s0 stream)
    (write-uint2 s1 stream)
    (write-uint2 s2 stream)
    (write-uint2 s3 stream))
  #-EXCL
  (write-byte-array x stream))

#-EXCL
(defun read-byte-array (n stream 
			&optional (eof-error-p t) (eof-value nil))
  (let ((b (make-array n :element-type '(unsigned-byte 8))))
    (if (> n (read-sequence b stream))
	(if eof-error-p (error "EOF") eof-value)
      b)))

#-EXCL
(defun write-byte-array (a stream)
  (write-sequence a stream))
