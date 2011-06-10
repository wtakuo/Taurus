;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; flags.cl
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

(defconstant *class-access-flags*
    '((#x0001 . :public)
      (#x0010 . :final)
      (#x0020 . :super)
      (#x0200 . :interface)
      (#x0400 . :abstract)))

(defconstant *field-access-flags*
    '((#x0001 . :public)
      (#x0002 . :private)
      (#x0004 . :protected)
      (#x0008 . :static)
      (#x0010 . :final)
      (#x0040 . :volatile)
      (#x0080 . :transient)))

(defconstant *method-access-flags*
    '((#x0001 . :public)
      (#x0002 . :private)
      (#x0004 . :protected)
      (#x0008 . :static)
      (#x0010 . :final)
      (#x0020 . :synchronized)
      (#x0100 . :native)
      (#x0400 . :abstract)
      (#x0800 . :strict)))

(defconstant *inner-class-access-flags*
    '((#x0001 . :public)
      (#x0002 . :private)
      (#x0004 . :protected)
      (#x0008 . :static)
      (#x0010 . :final)
      (#x0200 . :interface)
      (#x0400 . :abstract)))

(defun unpack-flags (bits alist)
  (loop
      for a in alist
      append
	(if (/= (logand (car a) bits) 0)
	    (list (cdr a))
	  '())))

(defun pack-flags (flags alist &aux (bits #x0000))
  (dolist (f flags)
    (let ((a (rassoc f alist)))
      (if a (setq bits (logior bits (car a))))))
  bits)
