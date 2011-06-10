;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; types.cl
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

;;; type ::= basic-type 
;;;        | (:array type) 
;;;        | (:ref name) 
;;;        | (:meth (type...) type)
;;; basic-type ::= :byte | :char | :double | :float 
;;;              | :int | :long | :short | :boolean
;;;              | :void

(defun unparse-type (type)
  (with-output-to-string (stream)
    (unparse-type-to-stream type stream)))

(defun unparse-type-to-stream (type &optional (stream *standard-output*))
  (if (listp type)
      (ecase (first type)
	(:array 
	 (write-char #\[ stream)
	 (unparse-type-to-stream (second type) stream))
	(:ref
	 (write-char #\L stream)
	 (write-string (second type) stream)
	 (write-char #\; stream))
	(:meth
	 (write-char #\( stream)
	 (dolist (x (second type))
	   (unparse-type-to-stream x stream))
	 (write-char #\) stream)
	 (unparse-type-to-stream (third type) stream)))
    (write-char (ecase type
		  (:byte #\B)
		  (:char #\C)
		  (:double #\D)
		  (:float #\F)
		  (:int #\I)
		  (:long #\J)
		  (:short #\S)
		  (:boolean #\Z)
		  (:void #\V))
		stream)))

(defun parse-type (s)
  (with-input-from-string (stream s)
    (parse-type-from-stream stream)))

(defun parse-type-from-stream (stream)
  (let ((c (read-char stream)))
    (ecase c
      (#\B :byte)
      (#\C :char)
      (#\D :double)
      (#\F :float)
      (#\I :int)
      (#\J :long)
      (#\S :short)
      (#\Z :boolean)
      (#\V :void)
      (#\[ (parse-array-type-from-stream stream))
      (#\( (parse-method-type-from-stream stream))
      (#\L (parse-reference-type-from-stream stream)))))

(defun parse-array-type-from-stream (stream)
  (list :array
	(parse-type-from-stream stream)))

(defun parse-method-type-from-stream (stream)
  (list :meth
	(loop
	    while (not (char= #\) (peek-char nil stream)))
	    collect (parse-type-from-stream stream)
	    finally (read-char stream))
	(parse-type-from-stream stream)))

(defun parse-reference-type-from-stream (stream)
  (list :ref
	(map 'string
	  #'identity
	  (loop
	      with c = nil
	      while (not (char= (setq c (read-char stream)) #\;))
	      collect 
		c))))

(defconstant *jvml-array-type-alist*
    '((4 . :boolean)
      (5 . :char)
      (6 . :float)
      (7 . :double)
      (8 . :byte)
      (9 . :short)
      (10 . :int)
      (11 . :long)
      ))

(defun get-array-type-name (b)
  (let ((a (assoc b *jvml-array-type-alist*)))
    (if a
	(cdr a)
      (error "invalid type specifier ~S" b))))

(defun get-array-type-code (type)
  (let ((a (rassoc type *jvml-array-type-alist*)))
    (if a
	(cdr a)
      (error "invalid type ~S" type))))

(defun mkreftype (type-name)
  (format nil "L~A;" type-name))

(defun make-java-type (type)
  (match type
    (:boolean "boolean")
    (:char "char")
    (:float "float")
    (:double "double")
    (:byte "byte")
    (:short "short")
    (:int "int")
    (:long "long")
    ((:ref _class) (substitute #\. #\/ _class))
    ((:array _type)  (format nil "~A[]" (make-java-type _type)))))
