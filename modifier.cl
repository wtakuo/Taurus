;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; modifier.cl
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

(defun add-field (ci name type flags)
  (let* ((cp (constant-pool ci))
	 (fld (make-instance 'field-info
		:class ci
		:access-flags flags
		:name-index (intern-cp-element cp :utf8 name)
		:descriptor-index (intern-cp-element cp :utf8 type)
		:attributes nil)))
    (setf (fields ci)
      (append (fields ci) (list fld)))))

(defun set-observer (ci
		     observer-type-name
		     observer-variable-name
		     observer-setter-name)
  (let* ((observer-type (format nil "L~A;" observer-type-name))
	 (observer-setter-type (format nil "(L~A;)V" observer-type-name))
	 (cp (constant-pool ci))
	 (code-attr
	  (make-instance 'code-attribute
	    :attribute-name-index (intern-cp-element 
				   cp 
				   :utf8 "Code")
	    :max-stack 2
	    :max-locals 2
	    :code (jvml-assemble 
		   `((L_start 
		      (I_ALOAD_0) 
		      (I_ALOAD_1)
		      (I_PUTFIELD 
		       (:FIELD ,(get-class-name ci)
			       ,observer-variable-name
			       ,observer-type))
		      (I_RETURN)))
		   ()
		   cp)
	    :exception-table nil
	    :attributes nil)))
    (add-field ci
	       observer-variable-name
	       observer-type
	       '(:public))
    (compute-attribute-length code-attr)
    (setf (methods ci)
      (append 
       (methods ci)
       (list
	(make-instance 'method-info
	  :class ci
	  :access-flags '(:public)
	  :name-index (intern-cp-element cp :utf8 observer-setter-name)
	  :descriptor-index (intern-cp-element cp
					       :utf8 observer-setter-type)
	  :attributes (list code-attr)))))))
	  
;;; tr t.f(t1, t2, ..., tn)
;;; void Observer.begin_f(t, t1, t2, ..., tn)
;;; void Observer.end_f(t, tr)


;;; invokevirtual target-method

;;; method-spec ::= (:method class-name method-name method-type)

(defun set-invokevirtual-observation (ci 
				      target-method-specs
				      observer-type-name
				      observer-variable-name)
  (dolist (mi (methods ci))
    (dolist (target-method-spec target-method-specs)
      (set-invokevirtual-observation1
       mi
       target-method-spec
       observer-type-name
       observer-variable-name))))

(defun set-invokevirtual-observation1 (mi
				       target-method-spec
				       observer-type-name
				       observer-variable-name)
  (plet ((:method _target-method-class
		  _target-method-name
		  _target-method-typespec)
	 target-method-spec)
    (let* ((ci (class-info mi))
	   (cp (constant-pool ci))
	   (ca (get-attribute mi "Code"))
	   (max-locals (max-locals ca))
	   (target-method-type (parse-type _target-method-typespec))
	   (nargs (length (second target-method-type))))
      (multiple-value-bind (code exc-tbl) (jvml-disassemble mi)
	(let ((new-code
	       (jvml-assemble 
		(mapcar #'(lambda (blk)
			    (insert-invokevirtual-observation
			     (get-class-name ci)
			     blk
			     _target-method-class
			     _target-method-name
			     _target-method-typespec
			     observer-type-name
			     observer-variable-name
			     max-locals))
			code)
		exc-tbl
		cp))
	      (new-max-stack (+ (max-stack ca) nargs 1))
	      (new-max-locals (+ max-locals nargs 1)))
	  (setf (code ca) new-code)
	  (setf (max-stack ca) new-max-stack)
	  (setf (max-locals ca) new-max-locals))))))

;;; xstore_k+n                  ; save n arguments
;;;    :
;;; xstore_k+1
;;; astore_k                    ; save object
;;; aload_0                     ; this
;;; getfield of                 ; get observer
;;; aload_k                     ; push object
;;; xload_k+1                   ; push n arguments
;;;    :
;;; xload_k+n
;;; invokevirtual method_index  ; observation method invocation
;;; aload_k                     ; push object
;;; xload_k+1                   ; push n arguments
;;;    :
;;; xload_k+n
;;; invokevirtual method_index  ; original method invocation

(defun insert-invokevirtual-observation (class-name
					 blk
					 target-method-class
					 target-method-name
					 target-method-type
					 observer-type-name
					 observer-variable-name
					 local-offset)
  (cons (car blk)			; label
	(loop
	    for line in (cdr blk)
	    append
	      (match line
		((i_invokevirtual (:method _CLASS _NAME _TYPE))
		 where (compatible-method-p _CLASS
					    _NAME 
					    _TYPE
					    target-method-class
					    target-method-name
					    target-method-type)
		 (plet ((:meth _ARG_TYPE _) (parse-type _TYPE))
		   (let ((args
			  (loop
			      with i = local-offset
			      for a in (cons (list :ref
						   (format nil "L~A;" _CLASS))
					     _ARG_TYPE)
			      collect (cons a i)
			      do (incf i)
				 (if (or (eq a :long) (eq a :double))
				     (incf i))))
			; (observer-type (format nil "L~A;"
			;			observer-type-name))
			 )
		     (append
		      (mapcar #'(lambda (p) (emit-xstore (car p) (cdr p)))
			      (reverse args))
		      (list
		       `(i_invokestatic
			 (:method ,observer-type-name
				  "getObserver"
				  ,(format nil "()L~A;" observer-type-name))))
		      ;(list
		      ; '(i_aload_0)
		      ; `(i_getfield (:field ,class-name
			;		    ,observer-variable-name
				;	    ,observer-type)))
		      (mapcar #'(lambda (p) (emit-xload (car p) (cdr p)))
			      args)
		      (list
		       `(i_invokevirtual 
			 (:method ,observer-type-name
				  ,(format nil "observe_~A" target-method-name)
				  ,(plet ((:meth _A _)
					  (parse-type target-method-type))
				     (unparse-type
				      (list :meth
					    (cons
					     (list :ref target-method-class)
					     _A)
					    :void))))))
		      (mapcar #'(lambda (p) (emit-xload (car p) (cdr p)))
			      args)
		      (list line)))))
		(_
		 (list line))))))

(defun compatible-method-p (c1 n1 t1 c2 n2 t2)
  (and (equal c1 c2)
       (equal n1 n2)
       (equal t1 t2)))

(defun emit-xstore (type i)
  (format t "~%xstore ~A ~A" type i)
  (let ((prefix
	 (if (listp type) 
	     "A"
	   (ecase type
	     ((:boolean :byte :char :short :int) "I")
	     (:float "F")
	     (:long "L")
	     (:double "D")))))
    (if (< i 4)
	(list (intern (format nil "I_~ASTORE_~A" prefix i)))
      (list (intern (format nil "I_~ASTORE" prefix))
	    (list :var i)))))

(defun emit-xload (type i)
  (format t "~%xload ~A ~A" type i)
  (let ((prefix
	 (if (listp type) 
	     "A"
	   (ecase type
	     ((:boolean :byte :char :short :int) "I")
	     (:float "F")
	     (:long "L")
	     (:double "D")))))
    (if (< i 4)
	(list (intern (format nil "I_~ALOAD_~A" prefix i)))
      (list (intern (format nil "I_~ALOAD" prefix))
	    (list :var i)))))


(defun mod-test (name)
  (let ((java-file (format nil "~A.java" name))
	(class-file (format nil "~A.class" name)))
    (compile-java-file java-file)
    (let* ((ci (load-class class-file))
	   (target-method-specs
	    '((:method "java/io/PrintStream" 
		       "println" "(Ljava/lang/String;)V")
	      (:method "Test10Target" "fact" "(I)I")))
	   (observer-type-name "Test10Observer")
	   (observer-variable-name "__OBSERVER__")
	   (observer-setter-name "setObserver"))
      (set-observer ci
		    observer-type-name
		    observer-variable-name
		    observer-setter-name)
      (set-invokevirtual-observation ci
				     target-method-specs
				     observer-type-name
				     observer-variable-name)
      (save-class ci class-file))))
