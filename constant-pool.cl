;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; constant-pool.cl
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

;;; JVM Constant Pool Rerepsentation
;;; <constant-pool> ::= (<cp-elem>...)
;;; <cp-elem> ::= (:class <index>)
;;;             | (:string <index>)
;;;             | (:field <index> <index>)
;;;             | (:method <index> <index>)
;;;             | (:interface-method <index> <index>)
;;;             | (:name-and-type <index> <index>)
;;;             | (:integer <integer>)
;;;             | (:float <bytes>)
;;;             | (:long <integer> <integer>)
;;;             | (:double <bytes>)
;;;             | (:utf8 <bytes>)
;;;             | (:unused)
;;;             | (:reserved)

(defconstant *const-tag-alist*
    '(( 1 . :utf8)
      ( 3 . :integer)
      ( 4 . :float)
      ( 5 . :long)
      ( 6 . :double)
      ( 7 . :class)
      ( 8 . :string)
      ( 9 . :field)
      (10 . :method)
      (11 . :interface-method)
      (12 . :name-and-type)))

(defun const-id-to-tag (id)
  (let ((e (assoc id *const-tag-alist*)))
    (if e
	(cdr e)
      (error "Invalid constant id: ~S" id))))

(defun const-tag-to-id (tag)
  (let ((e (rassoc tag *const-tag-alist*)))
    (if e
	(car e)
      (error "Invalid constant tag: ~s" tag))))

(defun read-constant-pool (stream)
  (let* ((cp-count (read-uint2 stream))
	 (cp (make-list cp-count))
	 (i 1))
    (setf (elt cp 0) (list :reserved))
    (loop
	while (< i cp-count)
	do
	  (let ((tag (const-id-to-tag (read-byte stream))))
	    (case tag
	      ((:integer)
	       (setf (elt cp i) (list :integer (read-int4 stream)))
	       (incf i))
	      ((:long)
	       (setf (elt cp i) (list :long (read-int8 stream)))
	       (incf i)
	       (setf (elt cp i) (list :unused))
	       (incf i))
	      ((:float)
	       (setf (elt cp i) (list :float (read-float stream)))
	       (incf i))
	      ((:double)
	       (setf (elt cp i) (list :double (read-double stream)))
	       (incf i)
	       (setf (elt cp i) (list :unused))
	       (incf i))
	      ((:class :string)
	       (setf (elt cp i) (list tag (read-uint2 stream)))
	       (incf i))
	      ((:field :method :interface-method :name-and-type)
	       (setf (elt cp i) (list tag
				      (read-uint2 stream)
				      (read-uint2 stream)))
	       (incf i))
	      ((:utf8)
	       (let* ((l (read-uint2 stream))
		      (b (make-array l :element-type '(unsigned-byte 8))))
		 (read-sequence b stream)
		 (setf (elt cp i) (list tag b)))
	       (incf i)))))
    cp))

(defun write-constant-pool (cp stream)
  (write-uint2 (length cp) stream)
  (dolist (elt cp)
    (let ((tag (car elt)))
      (ecase tag
	((:integer)
	 (write-byte (const-tag-to-id tag) stream)
	 (write-int4 (second elt) stream))
	((:long)
	 (write-byte (const-tag-to-id tag) stream)
	 (write-int8 (second elt) stream))
	((:float)
	 (write-byte (const-tag-to-id tag) stream)
	 (write-float (second elt) stream))
	((:double)
	 (write-byte (const-tag-to-id tag) stream)
	 (write-double (second elt) stream))
	((:class :string)
	 (write-byte (const-tag-to-id tag) stream)
	 (write-uint2 (second elt) stream))
	((:field :method :interface-method :name-and-type)
	 (write-byte (const-tag-to-id tag) stream)
	 (write-uint2 (second elt) stream)
	 (write-uint2 (third elt) stream))
	((:utf8)
	 (write-byte (const-tag-to-id tag) stream)
	 (write-uint2 (length (second elt)) stream)
	 (write-sequence (second elt) stream))
	((:unused :reserved))))))

(defun intern-cp-element (cp kind arg0 &rest args)
  (ecase kind
    ;; :class <index>
    ;; :class <string>
    ;; :string <index>
    ;; :string <string>
    ((:class :string)
     (cond ((numberp arg0)
	    (let* ((elem (list kind arg0))
		   (idx (position elem cp :test #'equal)))
	      (or idx
		  (prog1 (length cp)
		    (nconc cp (list elem))))))
	   ((stringp arg0)
	    (let ((idx (intern-cp-element cp :utf8 arg0)))
	      (intern-cp-element cp kind idx)))
	   (t (error "Invalid argument for ~S" kind))))
    ;; :field <index> <index>
    ;; :field <string> <string> <string>
    ;; :method <index> <index>
    ;; :method <string> <string> <string>
    ;; :interface-method <index> <index>
    ;; :interface-method <string> <string> <string>
    ((:field :method :interface-method)
     (cond ((numberp arg0)
	    (unless (= (length args) 1)
	      (error "Invalid argument for ~S" kind))
	    (let* ((arg1 (car args))
		   (elem (list kind arg0 arg1))
		   (idx (position elem cp :test #'equal)))
	      (or idx
		  (prog1 (length cp)
		    (nconc cp (list elem))))))
	   ((stringp arg0)
	    (unless (= (length args) 2)
	      (error "Invalid argument for ~S" kind))
	    (let* ((arg1 (car args))
		   (arg2 (cadr args))
		   (idx0 (intern-cp-element cp :class arg0))
		   (idx1 (intern-cp-element cp :name-and-type arg1 arg2)))
	      (intern-cp-element cp kind idx0 idx1)))
	   (t (error "Invalid argument for ~S" kind))))
    ;; :name-and-type <index> <index>
    ;; :name-and-type <string> <string>
    (:name-and-type
     (unless (= (length args) 1)
       (error "Invalid arguments for :name-and-type"))
     (let ((arg1 (car args)))
       (cond ((numberp arg0)
	      (let* ((elem (list :name-and-type arg0 arg1))
		     (idx (position elem cp :test #'equal)))
		(or idx
		    (prog1 (length cp)
		      (nconc cp (list elem))))))
	     ((stringp arg0)
	      (let ((idx0 (intern-cp-element cp :utf8 arg0))
		    (idx1 (intern-cp-element cp :utf8 arg1)))
		(intern-cp-element cp :name-and-type idx0 idx1)))
	     (t (error "Invalid arguments for :name-and-type")))))
    ;; :integer <integer>
    (:integer
     (let* ((elem (list :integer arg0))
	    (idx (position elem cp :test #'equal)))
       (or idx
	   (prog1 (length cp)
	     (nconc cp (list elem))))))
    ;; :long <integer>
    (:long
     (let* ((elem (list :long arg0))
	    (idx (position elem cp :test #'equalp)))
       (or idx
	   (prog1 (length cp)
	     (nconc cp (list elem (list :unused)))))))
    ;; :float <bytes>
    (:float
     (let* ((elem (list kind arg0))
	    (idx (position elem cp :test #'equalp)))
       (or idx
	   (prog1 (length cp)
	     (nconc cp (list elem))))))
    ;; :double <bytes>
    (:double
     (let* ((elem (list kind arg0))
	    (idx (position elem cp :test #'equalp)))
       (or idx
	   (prog1 (length cp)
	     (nconc cp (list elem (list :unused)))))))
    ;; :utf8 <string>
    (:utf8
     (let* ((elem (list :utf8 (make-utf8-bytes arg0)))
	    (idx (position elem cp :test #'equalp)))
       (or idx
	   (prog1 (length cp)
	     (nconc cp (list elem))))))))


(defun get-cp-element (cp index)
  (let ((elem (elt cp index)))
    (ecase (first elem)
      (:class
       (list :class
	     (second (get-cp-element cp (second elem)))))
      (:string
       (list :string
	     (second (get-cp-element cp (second elem)))))
      (:field
       (list* :field
	      (second (get-cp-element cp (second elem)))
	      (cdr (get-cp-element cp (third elem)))))
      (:method
       (list* :method
	      (second (get-cp-element cp (second elem)))
	      (cdr (get-cp-element cp (third elem)))))
      (:interface-method
       (list* :interface-method
	      (second (get-cp-element cp (second elem)))
	      (cdr (get-cp-element cp (third elem)))))
      (:name-and-type
       (list :name-and-type
	     (second (get-cp-element cp (second elem)))
	     (second (get-cp-element cp (third elem)))))
      (:integer
       (cons :integer (cdr elem)))
      (:float
       (cons :float (cdr elem)))
      (:long
       (cons :long (cdr elem)))
      (:double
       (cons :double (cdr elem)))
      (:utf8
       (list :utf8 (make-utf8-string (second elem))))
      ((:unused :reserved)
       elem))))

(defun make-utf8-string (bytes)
  (map 'string #'code-char bytes))

(defun make-utf8-bytes (string)
  (let* ((size (length string))
	 (b (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (elt b i) (char-code (elt string i))))
    b))


;;;
;;; for debugging and testing purpose
;;;

(defun dump-constant-pool (cp &optional (stream *standard-output*))
  (format stream "~%cp-count = ~S" (length cp))
  (loop
      for i from 1 to (1- (length cp)) do
	(format stream "~%~4S: ~S" i 
		(get-cp-element cp i))))

(defun load-constant-pool (path)
  (with-open-file (stream path
		   :element-type '(unsigned-byte 8))
    (read-constant-pool stream)))

(defun save-constant-pool (cp path)
  (with-open-file (stream path 
		   :direction :output
		   :if-exists :supersede
		   :element-type '(unsigned-byte 8))
    (write-constant-pool cp stream)))

