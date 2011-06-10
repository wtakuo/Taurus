;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; class.cl
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

(defun load-class (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (read-class stream)))

(defun save-class (ci path)
  (with-open-file (stream path :element-type '(unsigned-byte 8)
		   :direction :output
		   :if-exists :supersede)
    (write-contents ci stream)))

(defun read-class (stream)
  (let ((ci (make-instance 'class-info)))
    (read-contents ci stream)
    ci))

(defun check-magic (stream)
  (and (= (read-uint2 stream nil 0) #xCAFE)
       (= (read-uint2 stream nil 0) #xBABE)))


;;; ClassFile {
;;;   u4 magic;
;;;   u2 minor_version;
;;;   u2 major_version;
;;;   u2 constant_pool_count;
;;;   cp_info constant_pool[constant_pool_count-1];
;;;   u2 access_flags;
;;;   u2 this_class;
;;;   u2 super_class;
;;;   u2 interfaces_count;
;;;   u2 interfaces[interfaces_count];
;;;   u2 fields_count;
;;;   field_info fields[fields_count];
;;;   u2 methods_count;
;;;   method_info methods[methods_count];
;;;   u2 attributes_count;
;;;   attribute_info attributes[attributes_count];
;;; }

(defclass class-info ()
  ((version :accessor version
	    :initarg :version)
   (constant-pool :accessor constant-pool
		  :initarg :constant-pool)
   (access-flags :accessor access-flags
		 :initarg :access-flags)
   (this-class :accessor this-class
	       :initarg :this-class)
   (superclass :accessor superclass
	       :initarg :superclass)
   (interfaces :accessor interfaces
	       :initarg :interfaces)
   (fields :accessor fields
	   :initarg :fields)
   (methods :accessor methods
	    :initarg :methods)
   (attributes :accessor attributes
	       :initarg :attributes)))

(defmethod read-contents ((ci class-info) stream)
  ;; 1. magic number
  (unless (check-magic stream)
    (error "Bad magic number"))
  ;; 2. version
  (let ((minor (read-uint2 stream))
	(major (read-uint2 stream)))
    (setf (version ci) (list major minor)))
  ;; 4. constant pool
  (setf (constant-pool ci)
    (read-constant-pool stream))
  ;; 5. access flags
  (setf (access-flags ci) 
    (unpack-flags (read-uint2 stream) *class-access-flags*))
  ;; 6. this class
  (setf (this-class ci) 
    (read-uint2 stream))
  ;; 7. super class
  (setf (superclass ci) 
    (read-uint2 stream))
  ;; 8. interfaces
  (setf (interfaces ci)
    (loop
	with count = (read-uint2 stream)
	for i from 1 to count
	collect
	  (read-uint2 stream)))
  ;; 9. fields
  (setf (fields ci)
    (loop
	with count = (read-uint2 stream)
	for i from 1 to count
	collect
	  (read-field ci stream)))
  ;; 10. methods
  (setf (methods ci)
    (loop
	with count = (read-uint2 stream)
	for i from 1 to count
	collect
	  (read-method ci stream)))
  ;; 11. attributes
  (setf (attributes ci) 
    (read-attributes (constant-pool ci) stream))
  ;; 
  ci)

(defmethod write-contents ((ci class-info) stream)
  ;; 1. magic number
  (write-uint2 #xCAFE stream)
  (write-uint2 #xBABE stream)
  ;; 2. version
  (write-uint2 (second (version ci)) stream)  ; minor
  (write-uint2 (first (version ci)) stream) ; major
  ;; 4. constant pool
  (write-constant-pool (constant-pool ci) stream)
  ;; 5. access flags
  (write-uint2 
   (pack-flags (access-flags ci) *class-access-flags*)
   stream)
  ;; 6. this class
  (write-uint2 (this-class ci) stream)
  ;; 7. superclass
  (write-uint2 (superclass ci) stream)
  ;; 8. interfaces
  (write-uint2 (length (interfaces ci)) stream)
  (dolist (idx (interfaces ci))
    (write-uint2 idx stream))
  ;; 9. fields
  (write-uint2 (length (fields ci)) stream)
  (dolist (fi (fields ci))
    (write-contents fi stream))
  ;; 10. methods
  (write-uint2 (length (methods ci)) stream)
  (dolist (mi (methods ci))
    (write-contents mi stream))
  ;; 11. attributes
  (write-attributes (attributes ci) stream)
  )

(defmethod get-class-name ((ci class-info))
  (let ((cp (constant-pool ci)))
    (cadr (get-cp-element cp (this-class ci)))))

(defmethod get-attribute ((ci class-info) attr-name)
  (let ((cp (constant-pool ci)))
    (dolist (attr (attributes ci))
      (let ((e (get-cp-element cp (attribute-name-index attr))))
	(if (and (eq (first e) :utf8)
		 (equal (second e) attr-name))
	    (return-from get-attribute attr))))
    nil))

(defmethod find-field-info ((ci class-info) field-name)
  (let ((cp (constant-pool ci)))
    (dolist (fi (fields ci))
      (let ((e (get-cp-element cp (name-index fi))))
	(if (and (eq (first e) :utf8)
		 (equal (second e) field-name))
	    (return-from find-field-info fi))))
    nil))

(defmethod find-method-info ((ci class-info) method-name &optional method-type)
  (let ((cp (constant-pool ci)))
    (dolist (mi (methods ci))
      (let ((ne (get-cp-element cp (name-index mi)))
	    (de (get-cp-element cp (descriptor-index mi))))
	(if (and (eq (first ne) :utf8)
		 (equal (second ne) method-name)
		 (or (not method-type)
		     (and (eq (first de) :utf8)
			  (equal (second de) method-type))))
	    (return-from find-method-info mi))))
    nil))

(defmethod dump ((ci class-info) &optional (stream *standard-output*))
  (let ((cp (constant-pool ci)))
    (format stream "~%version: ~S" (version ci))
    (dump-constant-pool cp stream)
    (format stream "~%access-flags: ~S" (access-flags ci))
    (format stream "~%this-class: ~S" 
	    (second (get-cp-element cp (this-class ci))))
    (format stream "~%superclass: ~S" 
	    (second (get-cp-element cp (superclass ci))))
    (format stream "~%interfaces: ~S" 
	    (mapcar #'(lambda (idx) (second (get-cp-element cp idx)))
		    (interfaces ci)))
    (dolist (field (fields ci)) 
      (dump field stream))
    (dolist (method (methods ci)) 
      (dump method stream))
    (dump-attributes (attributes ci) cp stream)))


;;; Class Contents

(defclass class-content ()
  ((class :accessor class-info
	  :initarg :class)))

(defmethod constant-pool ((cc class-content))
  (constant-pool (class-info cc)))

(defmethod get-attribute ((cc class-content) attr-name)
  (let ((cp (constant-pool cc)))
    (dolist (attr (attributes cc))
      (let ((e (get-cp-element cp (attribute-name-index attr))))
	(if (and (eq (first e) :utf8)
		 (equal (second e) attr-name))
	    (return-from get-attribute attr))))))

;;; field_info {
;;;   u2 access_flags;
;;;   u2 name_index;
;;;   u2 descriptor_index;
;;;   u2 attributes_count;
;;;   attribute_info attributes[attributes_count];
;;; }

(defclass field-info (class-content)
  ((access-flags :accessor access-flags
		 :initarg :access-flags)
   (name-index :accessor name-index
	       :initarg :name-index)
   (descriptor-index :accessor descriptor-index
		     :initarg :descriptor-index)
   (attributes :accessor attributes
	       :initarg :attributes)))

(defun read-field (ci stream)
  (let ((field (make-instance 'field-info :class ci)))
    (read-contents field stream)
    field))

(defmethod read-contents ((fi field-info) stream)
  (setf (access-flags fi)
    (unpack-flags (read-uint2 stream) *field-access-flags*))
  (setf (name-index fi) (read-uint2 stream))
  (setf (descriptor-index fi) (read-uint2 stream))
  (setf (attributes fi) 
    (read-attributes (constant-pool fi) stream)))

(defmethod write-contents ((fi field-info) stream)
  (write-uint2 (pack-flags (access-flags fi) *field-access-flags*)
	       stream)
  (write-uint2 (name-index fi) stream)
  (write-uint2 (descriptor-index fi) stream)
  (write-attributes (attributes fi) stream))

(defmethod dump ((fi field-info) &optional (stream *standard-output*))
  (let ((cp (constant-pool fi)))
    (format stream
	    "~%field: ~S ~S ~S "
	    (access-flags fi)
	    (second (get-cp-element cp (name-index fi)))
	    (second (get-cp-element cp (descriptor-index fi))))
    (dump-attributes (attributes fi) cp stream)))


;;; method_info {
;;;   u2 access_flags;
;;;   u2 name_index;
;;;   u2 descriptor_index;
;;;   u2 attributes_count;
;;;   attribute_info attributes[attributes_count];
;;; }

(defclass method-info (class-content)
  ((access-flags :accessor access-flags
		 :initarg :access-flags)
   (name-index :accessor name-index
	       :initarg :name-index)
   (descriptor-index :accessor descriptor-index
		     :initarg :descriptor-index)
   (attributes :accessor attributes
	       :initarg :attributes)))

(defun read-method (ci stream)
  (let ((method (make-instance 'method-info :class ci)))
    (read-contents method stream)
    method))

(defmethod read-contents ((mi method-info) stream)
  (setf (access-flags mi)
    (unpack-flags (read-uint2 stream) *method-access-flags*))
  (setf (name-index mi) (read-uint2 stream))
  (setf (descriptor-index mi) (read-uint2 stream))
  (setf (attributes mi) 
    (read-attributes (constant-pool  mi) stream)))

(defmethod write-contents ((mi method-info) stream)
  (write-uint2 (pack-flags (access-flags mi) *method-access-flags*)
	       stream)
  (write-uint2 (name-index mi) stream)
  (write-uint2 (descriptor-index mi) stream)
  (write-attributes (attributes mi) stream))

(defmethod get-code ((mi method-info))
  (code (get-attribute mi "Code")))

(defmethod get-max-stack ((mi method-info))
  (max-stack (get-attribute mi "Code")))

(defmethod get-max-locals ((mi method-info))
  (max-locals (get-attribute mi "Code")))

(defmethod get-exception-table ((mi method-info))
  (exception-table (get-attribute mi "Code")))

(defmethod get-exceptions ((mi method-info))
  (get-attribute mi "Exceptions"))

(defmethod get-line-number-table ((mi method-info))
  (get-attribute mi "LineNumberTable"))

(defmethod dump ((mi method-info) &optional (stream *standard-output*))
  (let ((cp (constant-pool mi)))
    (format stream
	    "~%method: ~S ~S ~S "
	    (access-flags mi)
	    (second (get-cp-element cp (name-index mi)))
	    (second (get-cp-element cp (descriptor-index mi))))
    (dump-attributes (attributes mi) cp stream)))
