;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; attributes.cl
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

(defparameter *known-attributes-alist*
    '(("ConstantValue" . constant-value-attribute)
      ("Code" . code-attribute)
      ("Exceptions" . exceptions-attribute)
      ("InnerClasses" . inner-classes-attribute)
      ("Synthetic" . synthetic-attribute)
      ("SourceFile" . source-file-attribute)
      ("LineNumberTable" . line-number-table-attribute)
      ("LocalVariableTable" . local-variable-table-attribute)
      ("Deprecated" . deprecated-attribute)))


(defun read-attributes (cp stream)
  (loop
      with count = (read-uint2 stream)
      for i from 1 to count
      collect
	(read-attribute cp stream)))

(defun read-attribute (cp stream)
  (let* ((name-index (read-uint2 stream))
	 (length (read-uint4 stream))
	 (attr (make-instance (find-attribute-class cp name-index)
		 :attribute-name-index name-index	       
		 :attribute-length length)))
    (read-attribute-contents attr cp stream)
    attr))

(defun write-attributes (attributes stream)
  (write-uint2 (length attributes) stream)
  (dolist (attr attributes)
    (write-attribute attr stream)))

(defun dump-attributes (attrs cp &optional (stream *standard-output*))
  (dolist (attr attrs)
    (dump-attribute attr cp stream)))


(defun find-attribute-class (cp idx)
  (let* ((cp-elem (get-cp-element cp idx))
	 (a))
    (unless (eql (car cp-elem) :utf8)
      (error "read-attribute: bad name-index ~S (~S)" idx cp-elem))
    (setq a (assoc (second cp-elem)
		   *known-attributes-alist*
		   :test #'equal))
    (if a (cdr a) 'attribute)))



;;; attribute_info {
;;;   u2 attribute_name_index;
;;;   u4 attribute_length;
;;;   u1 info[attribute_length];
;;; }

(defclass attribute ()
  ((attribute-name-index :accessor attribute-name-index
			 :initarg :attribute-name-index)
   (attribute-length :accessor attribute-length
		     :initarg :attribute-length
		     :initform 0)
   (info :accessor info
	 :initarg :info)))

(defmethod read-attribute-contents ((attr attribute) cp stream)
  (declare (ignore cp))
  (let ((b (make-array (attribute-length attr)
		       :element-type '(unsigned-byte 8))))
    (read-sequence b stream)
    (setf (info attr) b)))

(defmethod write-attribute :before ((attr attribute) stream)
  (compute-attribute-length attr)
  (write-uint2 (attribute-name-index attr) stream)
  (write-uint4 (attribute-length attr) stream))

(defmethod write-attribute ((attr attribute) stream)
  (write-sequence (info attr) stream))

;(defmethod compute-attribute-length :before ((attr attribute))
;  (format t "~%~A: ~A" attr (attribute-length attr)))

;(defmethod compute-attribute-length :after ((attr attribute))
;  (format t "~%~A: ~A" attr (attribute-length attr)))

(defmethod compute-attribute-length ((attr attribute))
  (setf (attribute-length attr) (length (info attr))))

(defmethod dump-attribute :before ((attr attribute) cp
				   &optional (stream *standard-output*))
  (format stream "~%~A=" 
	  (second (get-cp-element cp (attribute-name-index attr)))))

(defmethod dump-attribute ((attr attribute) cp
			   &optional (stream *standard-output*))
  (declare (ignore cp))    
  (format stream "~S" (info attr)))


;;; ConstantValue_attribute {
;;;   u2 attribute_name_index;  -> (:utf8 "ConstantValue")
;;;   u4 attribute_length;      = 2
;;;   u2 constantvalue_index;
;;; }

(defclass constant-value-attribute (attribute)
  ((constant-value-index :accessor constant-value-index
			 :initarg :constant-value-index)))

(defmethod read-attribute-contents ((attr constant-value-attribute)
				    cp stream)
  (declare (ignore cp))
  (unless (= (attribute-length attr) 2)
    (error "constant-value-attribute: invalid attribute length ~S"
	   (attribute-length attr)))
  (setf (constant-value-index attr) (read-uint2 stream)))

(defmethod write-attribute ((attr constant-value-attribute) stream)
  (write-uint2 (constant-value-index attr) stream))

(defmethod compute-attribute-length ((attr constant-value-attribute))
  (setf (attribute-length attr) 2))

(defmethod dump-attribute ((attr constant-value-attribute) cp
			   &optional (stream *standard-output*))
  (format stream "~S" 
	  (get-cp-element cp (constant-value-index attr))))


;;; Code_attribute {
;;;   u2 attribute_name_index;  -> (:utf8 "Code")
;;;   u4 attribute_length;
;;;   u2 max_stack;
;;;   u2 max_locals;
;;;   u4 code_length;
;;;   u1 code[code_length];
;;;   u2 exception_table_length;
;;;   { u2 start_pc;
;;;     u2 end_pc;
;;;     u2 handler_pc;
;;;     u2 catch_type;
;;;   } exception_table[exception_table_length];
;;;   u2 attributes_count;
;;;   attribute_info attributes[attributes_count];
;;; }

(defclass code-attribute (attribute)
  ((max-stack :accessor max-stack
	      :initarg :max-stack)
   (max-locals :accessor max-locals
	       :initarg :max-locals)
   (code :accessor code
	 :initarg :code)
   (exception-table :accessor exception-table
		    :initarg :exception-table)
   (attributes :accessor attributes
	       :initarg :attributes)))

(defmethod read-attribute-contents ((attr code-attribute) cp stream)
  ;; max-stack
  (setf (max-stack attr) (read-uint2 stream))
  ;; max-locals
  (setf (max-locals attr) (read-uint2 stream))
  ;; code
  (let* ((code-length (read-uint4 stream))
	 (b (make-array code-length :element-type '(unsigned-byte 8))))
    (read-sequence b stream)
    (setf (code attr) b))
  ;; exception-table
  (setf (exception-table attr)
    (loop
	with exception-table-length = (read-uint2 stream)
	for i from 1 to exception-table-length
	collect
	  (list (read-uint2 stream) ; start_pc
		(read-uint2 stream) ; end_pc
		(read-uint2 stream) ; handler_pc
		(read-uint2 stream) ; catch_type
		)))
  ;; attributes
  (setf (attributes attr) (read-attributes cp stream)))

(defmethod write-attribute ((attr code-attribute) stream)
  (write-uint2 (max-stack attr) stream)
  (write-uint2 (max-locals attr) stream)
  (write-uint4 (length (code attr)) stream)
  (write-sequence (code attr) stream)
  (write-uint2 (length (exception-table attr)) stream)
  (dolist (e (exception-table attr))
    (write-uint2 (first e) stream)
    (write-uint2 (second e) stream)
    (write-uint2 (third e) stream)
    (write-uint2 (fourth e) stream))
  (write-attributes (attributes attr) stream))

(defmethod compute-attribute-length ((attr code-attribute))
  (setf (attribute-length attr)
    (+ 2				; max_stack
       2				; max_locals
       4				; code_length
       (length (code attr))		; code
       2				; exception_table_length
       (* 8 (length (exception-table attr))) ; exception_table
       2				; attributes_count
       (loop				; attributes
	   for attr in (attributes attr)
	   sum 
	     (+ 2			; attribute_name_index
		4			; attribute_length
		(compute-attribute-length attr))))))

(defmethod dump-attribute ((attr code-attribute) cp
			   &optional (stream *standard-output*))
  (format stream "stack:~S locals:~S ~S exceptions:~S"
	  (max-stack attr)
	  (max-locals attr)
	  (code attr)
	  (mapcar #'(lambda (e)
		      (list (first e)
			    (second e)
			    (third e)
			    (if (= (fourth e) 0)
				:any
			      (get-cp-element cp (fourth e)))))
		  (exception-table attr)))
  (dump-attributes (attributes attr) cp stream))


;;; Exceptions_attribute {
;;;   u2 attribute_name_index;  -> (:utf8 "Exceptions")
;;;   u4 attribute_length;      = number_of_exceptions * 2 + 2
;;;   u2 number_of_exceptions;
;;;   u2 exception_index_table[number_of_exceptions];
;;; }

(defclass exceptions-attribute (attribute)
  ((exception-index-table :accessor exception-index-table
			  :initarg :exception-index-table)))

(defmethod read-attribute-contents ((attr exceptions-attribute) cp stream)
  (declare (ignore cp))
  ;; exception-index-table
  (setf (exception-index-table attr)
    (loop
	with number-of-exceptions = (read-uint2 stream)
	for i from 1 to number-of-exceptions
	collect
	  (read-uint2 stream))))

(defmethod write-attribute ((attr exceptions-attribute) stream)
  (write-uint2 (length (exception-index-table attr)) stream)
  (dolist (idx (exception-index-table attr))
    (write-uint2 idx stream)))

(defmethod compute-attribute-length ((attr exceptions-attribute))
  (setf (attribute-length attr)
    (+ 2				; number_of_exceptions
       (* 2 (length (exception-index-table attr))))))

(defmethod dump-attribute ((attr exceptions-attribute) cp
			   &optional (stream *standard-output*))
  (format stream "~S" 
	  (mapcar #'(lambda (idx)
		      (second (get-cp-element cp idx)))
		  (exception-index-table attr))))


;;; InnerClasses_attribute {
;;;   u2 attribute_name_index;  -> (:utf8 "InnerClasses")
;;;   u4 attribute_length;      = number_of_classes * 8 + 2
;;;   u2 number_of_classes;
;;;   {  u2 inner_class_info_index;        
;;;      u2 outer_class_info_index;        
;;;      u2 inner_name_index;      
;;;      u2 inner_class_access_flags;      
;;;   } classes[number_of_classes];
;;; }

(defclass inner-classes-attribute (attribute)
  ((classes :accessor classes
	    :initarg :classes)))

(defmethod read-attribute-contents ((attr inner-classes-attribute) 
				    cp stream)
  (declare (ignore cp))
  ;; classes
  (setf (classes attr)
    (loop
	with number-of-classes = (read-uint2 stream)
	for i from 1 to number-of-classes
	collect
	  (list (read-uint2 stream) ; inner_class_info_index
		(read-uint2 stream) ; outer_class_info_index
		(read-uint2 stream) ; inner_name_index
		(unpack-flags (read-uint2 stream) ; inner_class_access_flags
			      *inner-class-access-flags*)
		))))

(defmethod write-attribute ((attr inner-classes-attribute) stream)
  (write-uint2 (length (classes attr)) stream)
  (dolist (c (classes attr))
    (write-uint2 (first c) stream)
    (write-uint2 (second c) stream)
    (write-uint2 (third c) stream)
    (write-uint2 (pack-flags (fourth c) *inner-class-access-flags*)
		 stream)))

(defmethod compute-attribute-length ((attr inner-classes-attribute))
  (setf (attribute-length attr)
    (+ 2				; number_of_classes
       (* 8 (length (classes attr))))))

(defmethod dump-attribute ((attr inner-classes-attribute) cp
			   &optional (stream *standard-output*))
  (declare (ignore cp))
  (format stream "~S"
	  (classes attr)))


;;; Synthetic_attribute {
;;;   u2 attribute_name_index;  -> (:utf8 "Synthetic")
;;;   u4 attribute_length;      = 0
;;; }

(defclass synthetic-attribute (attribute)
  ())

(defmethod read-attribute-contents ((attr synthetic-attribute) cp stream)
  (declare (ignore cp stream))
  (unless (= (attribute-length attr) 0)
    (error "synthetic-attribute: invalid size ~S"
	   (attribute-length attr))))

(defmethod write-attribute ((attr synthetic-attribute) stream)
  (declare (ignore stream))
  )

(defmethod compute-attribute-length ((attr synthetic-attribute))
  (setf (attribute-length attr) 0))

(defmethod dump-attribute ((attr synthetic-attribute) cp
			   &optional (stream *standard-output*))
  (declare (ignore cp))
  (format stream "-"))


;;; SourceFile_attribute {
;;;   u2 attribute_name_index;  -> (:utf8 "SourceFile")
;;;   u4 attribute_length;      = 2
;;;   u2 sourcefile_index;
;;; }

(defclass source-file-attribute (attribute)
  ((source-file-index :accessor source-file-index
		      :initarg :source-file-index)))

(defmethod read-attribute-contents ((attr source-file-attribute) cp stream)
  (declare (ignore cp))
  (unless (= (attribute-length attr) 2)
    (error "source-file-attribute: invalid attribute size ~S"
	   (attribute-length attr)))
  (setf (source-file-index attr) (read-uint2 stream)))

(defmethod write-attribute ((attr source-file-attribute) stream)
  (write-uint2 (source-file-index attr) stream))

(defmethod compute-attribute-length ((attr source-file-attribute))
  (setf (attribute-length attr) 2))

(defmethod dump-attribute ((attr source-file-attribute) cp
			   &optional (stream *standard-output*))
  (format stream "~S"
	  (second (get-cp-element cp (source-file-index attr)))))


;;; LineNumberTable_attribute {
;;;   u2 attribute_name_index;  -> (:utf8 "LineNumberTable")
;;;   u4 attribute_length;
;;;   u2 line_number_table_length;
;;;   { u2 start_pc;
;;;     u2 line_number;
;;;   } line_number_table[line_number_table_length];
;;; }

(defclass line-number-table-attribute (attribute)
  ((line-number-table :accessor line-number-table
		      :initarg :line-number-table)))

(defmethod read-attribute-contents ((attr line-number-table-attribute) 
				    cp stream)
  (declare (ignore cp))
  (setf (line-number-table attr)
    (loop
	with line-number-table-length = (read-uint2 stream)
	for i from 1 to line-number-table-length
	collect
	  (list (read-uint2 stream) ; start_pc
		(read-uint2 stream) ; line_number
		))))

(defmethod write-attribute ((attr line-number-table-attribute) stream)
  (write-uint2 (length (line-number-table attr)) stream)
  (dolist (l (line-number-table attr))
    (write-uint2 (first l) stream)
    (write-uint2 (second l) stream)))

(defmethod compute-attribute-length ((attr line-number-table-attribute))
  (setf (attribute-length attr)
    (+ 2				; line_number_table_length
       (* 4 (length (line-number-table attr))))))

(defmethod dump-attribute ((attr line-number-table-attribute) cp
			   &optional (stream *standard-output*))
  (declare (ignore cp))
  (format stream "~S"
	  (line-number-table attr)))


;;; LocalVariableTable_attribute {
;;;   u2 attribute_name_index;  -> (:utf8 "LocalVariableTable")
;;;   u4 attribute_length;      = 10*local_variable_table_length
;;;   u2 local_variable_table_length;
;;;   { u2 start_pc;
;;;     u2 length;
;;;     u2 name_index;
;;;     u2 descriptor_index;
;;;     u2 index;
;;;   } local_variable_table[local_variable_table_length];
;;; }

(defclass local-variable-table-attribute (attribute)
  ((local-variable-table :accessor local-variable-table
			 :initarg :local-variable-table)))

(defmethod read-attribute-contents ((attr local-variable-table-attribute)
				    cp stream)
  (declare (ignore cp))
  (setf (local-variable-table attr)
    (loop
	with local-variable-table-length = (read-uint2 stream)
	for i from 1 to local-variable-table-length
	collect
	  (list (read-uint2 stream) ; start_pc
		(read-uint2 stream) ; length
		(read-uint2 stream) ; name_index
		(read-uint2 stream) ; descriptor_index
		(read-uint2 stream) ; index
		))))

(defmethod write-attribute ((attr local-variable-table-attribute) stream)
  (write-uint2 (length (local-variable-table attr)) stream)
  (dolist (l (local-variable-table attr))
    (write-uint2 (first l) stream)
    (write-uint2 (second l) stream)
    (write-uint2 (third l) stream)
    (write-uint2 (fourth l) stream)
    (write-uint2 (fifth l) stream)))

(defmethod compute-attribute-length ((attr local-variable-table-attribute))
  (setf (attribute-length attr)
    (+ 2				; local_variable_table_length
       (* 10 (length (local-variable-table attr))))))

(defmethod dump-attribute ((attr local-variable-table-attribute) cp
			   &optional (stream *standard-output*))
  (declare (ignore cp))
  (format stream "~S"
	  (local-variable-table attr)))


;;; Deprecated_attribute {
;;;   u2 attribute_name_index;  -> (:utf8 "Deprecated")
;;;   u4 attribute_length;      = 0
;;; }

(defclass deprecated-attribute (attribute)
  ())

(defmethod read-attribute-contents ((attr deprecated-attribute) cp stream)
  (declare (ignore cp stream))
  (unless (= (attribute-length attr) 0)
    (error "deprecated-attribute: invalid size ~S"
	   (attribute-length attr))))

(defmethod write-attribute ((attr deprecated-attribute) stream)
  (declare (ignore stream))
  )

(defmethod compute-attribute-length ((attr deprecated-attribute))
  (setf (attribute-length attr) 0))

(defmethod dump-attribute ((attr deprecated-attribute) cp
			   &optional (stream *standard-output*))
  (declare (ignore cp))
  (format stream "-"))
