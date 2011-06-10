;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; observer.cl
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

(defun gen-observer (observer-class-name
		     method-specs
		     stream)
  (format stream "~%class ~A {" observer-class-name)
  (dolist (method-spec method-specs)
    (gen-observe-method method-spec stream))
  (format stream "~%}"))

(defun gen-observe-method (method-spec stream)
  (plet ((:method _class _name _typespec) method-spec)
    (format stream "~%void observe_~A (~A a0" 
	    _name (substitute #\. #\/ _class))
    (gen-observer-args (second (parse-type _typespec)) stream)
    (format stream ") {")
    
    (format stream "~%}")))
	    
(defun gen-observer-args (types stream &aux i)
  (setq i 1)
  (dolist (type types)
    (format stream ", ~A a~A" (make-java-type type) i)
    (incf i)))
