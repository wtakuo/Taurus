;;; -*- Mode: Common-Lisp; -*-
;;;
;;; load.cl
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

(defpackage :taurus
  (:use 
   :common-lisp 
   #+EXCL :excl
   #+CCL :ccl
   #+CLISP :ext
   )
  (:export
   #:load-taurus
   #:build-taurus
   ))

(in-package :taurus)

(provide :taurus)

(defparameter *taurus-version* '(0 0 1))

(defparameter *taurus-version-string*
    (format nil "~d.~d.~d"
	    (car *taurus-version*)
	    (cadr *taurus-version*)
	    (caddr *taurus-version*)))

(defparameter *debug* nil)

(defparameter *taurus-root* 
    (directory-namestring *load-truename*))

(defparameter *taurus-files*
    '(
      "system"
      "pmacros"
      "byteio"
      "types"
      "constant-pool"
      "flags"
      "attributes"
      "class"
      "jvml-instructions"
      "jvml-disassembler"
      "jvml-assembler"
      "flow"
      "modifier"
      "test"
      ))

(defparameter *src-extension* ".cl")

(defparameter *fasl-extension* 
    #+EXCL  ".fasl"
    #+CCL   ".dfsl"
    #+CLISP ".fas"
    )

(defun load-taurus (&key (src nil) (verbose *load-verbose*))
  (dolist (file *taurus-files*)
    (let ((fasl-file (taurus-fasl-file file))
	  (src-file (taurus-src-file file)))
      (cond ((and (not src) (probe-file fasl-file))
	     (load fasl-file :verbose verbose))
	    ((probe-file src-file)
	     (load src-file :verbose verbose))
	    (t
	     (error "cannot file file ~S" src-file))))))

(defun build-taurus (&key (verbose *compile-verbose*))
  (dolist (file *taurus-files*)
    (compile-file (taurus-src-file file) :verbose verbose)))

(defun clean-fasl-files ()
  (dolist (file *taurus-files*)
    (let ((fasl-file (taurus-fasl-file file))
	  #+CLISP
	  (lib-file (taurus-lib-file file))
	  )
      (when (probe-file fasl-file)
	(format t "~%deleting ~S" fasl-file)
	(delete-file fasl-file))
      #+CLISP
      (when (probe-file lib-file)
	(format t "~%deleting ~S" lib-file)
	(delete-file lib-file))
      )))

(defun taurus-src-file (file)
  (format nil "~A~A~A" *taurus-root* file *src-extension*))

(defun taurus-fasl-file (file)
  (format nil "~A~A~A" *taurus-root* file *fasl-extension*))

#+CLISP
(defun taurus-lib-file (file)
  (format nil "~A~A.lib" *taurus-root* file))

;(eval-when (load eval)
;  (load-taurus)
;  )
