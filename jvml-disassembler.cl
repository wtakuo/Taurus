;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; jvml-disassembler.cl
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

;;; disassembled code
;;;    code ::= (block ...)
;;;    block ::= (label line ...)
;;;    line ::= (instr arg ...)

(defun jvml-disassemble (mi)
  "mi -> code * exc"
  (let ((cp (constant-pool mi))
	(lbltbl (list :dummy))
    	(exception-table nil)
	(code1 nil)
	(code2 nil))
    ;; label exceptions
    (setq exception-table
      (loop
	  for exc in (get-exception-table mi)
	  collect
	    ;; exc = (start end target type-index)
	    (let ((start-lbl (intern-address lbltbl (first exc)))
		  (end-lbl (intern-address lbltbl (second exc)))
		  (target-lbl (intern-address lbltbl (third exc)))
		  (exc-type (if (zerop (fourth exc))
				:any
			      (get-cp-element cp (fourth exc)))))
	      (list start-lbl end-lbl target-lbl exc-type))))
    ;; label line arguments
    (setq code1
      (loop
	  for line in (jvml-disassemble1 (get-code mi))
	  collect
	    (label-line-args line lbltbl cp)))
    ;; label lines
    (setq code2
      (loop
	  for line in code1
	  append
	    (let* ((lbl (find-label lbltbl (car line))))
	      (if lbl
		  (list lbl (cdr line))
		(list (cdr line))))))
    (values (blockify code2) exception-table)))

;;; unblockified code
;;;   code ::= (line ...)
;;;   line ::= label | (mnemonic arg ...)

(defun blockify (code)
  (blockify1 (gentemp "M") code nil nil))

(defun blockify1 (label code block sofar)
  (if (null code)
      (nreverse
       (if (null block)
	   sofar
	 (cons (list* label (nreverse block)) sofar)))
    (let ((line (car code)))
      (cond ((symbolp line)
	     (blockify1 line 
			(cdr code) 
			()
			(if (null block)
			    sofar
			  (cons
			   (list* label (nreverse block))
			   sofar))))
	    ((member (car line) *jvml-block-tail-instructions*)
	     (let ((newlabel (gentemp "M")))
	       (blockify1 newlabel
			  (cdr code) 
			  ()
			  (cons
			   (list* label (nreverse (cons line block)))
			   sofar))))
	    (t
	     (blockify1 label 
			(cdr code) 
			(cons line block) 
			sofar))))))


(defun label-line-args (line lbltbl cp)
  (let ((addr (first line))
	(mnemonic (second line))
	(args (cddr line)))
    (list* addr
	   mnemonic
	   (case mnemonic
	     ;; args = (npadding default low high offset1 ...)
	     (i_tableswitch
	      (list*
	       (label-offset32 lbltbl addr (cadr args))
	       (caddr args)
	       (cadddr args)
	       (mapcar #'(lambda (a)
			   (label-offset32 lbltbl addr a))
		       (cddddr args))))
	     ;; args = (npadding default npairs (value1 offset1) ...)
	     (i_lookupswitch
	      (list*
	       (label-offset32 lbltbl addr (cadr args))
	       (caddr args)
	       (mapcar #'(lambda (p)
			   (list (car p)
				 (label-offset32 lbltbl addr (cadr p))))
		       (cdddr args))))
	     (t 
	      (mapcar #'(lambda (arg type)
			  (ecase type
			    (n (list :val arg))
			    (i (get-cp-element cp arg))
			    (v (list :var arg))
			    (x (get-array-type-name arg))
			    (o (label-offset16 lbltbl addr arg))
			    (w (label-offset32 lbltbl addr arg))))
		      args
		      (instr-argtype (jvml-m2i mnemonic))))))))

(defun label-offset16 (lbltbl addr offset)
  (intern-address lbltbl 
		  (+ addr (uint2-to-int2 offset))))

(defun label-offset32 (lbltbl addr offset)
  (intern-address lbltbl 
		  (+ addr (uint4-to-int4 offset))))

(defun find-label (lbltbl addr)
  (let ((a (rassoc addr (cdr lbltbl))))
    (if a
	(car a)
      nil)))
     
(defun intern-address (lbltbl addr)
  (let ((a (rassoc addr (cdr lbltbl))))
    (if a
	(car a)
      (let ((lbl (gentemp "L")))
	(nconc lbltbl (list (cons lbl addr)))
	lbl))))


;;; Disassembler (1st pass)
;;; raw-code = ((addr instr arg ...) ...)

(defun jvml-disassemble1 (bcode)
  (let ((len (length bcode))
	(pc 0)
	(raw-code '()))
    (loop
	while (< pc len)
	do
	  (let* ((instr (jvml-b2i (elt bcode pc)))
		 (argspec (instr-argspec instr)))
	    (if (eq argspec ':special)
		(multiple-value-bind (line newpc)
		    (ecase (instr-mnemonic instr)
		      (i_wide
		       (jvml-disassemble1-wide bcode pc))
		      (i_tableswitch
		       (jvml-disassemble1-tableswitch bcode pc))
		      (i_lookupswitch
		       (jvml-disassemble1-lookupswitch bcode pc)))
		  (setq raw-code
		    (nconc raw-code (list line)))
		  (setq pc newpc))
	      (let* ((pc0 (1+ pc)) 
		     (raw-args
		      (loop
			  for argbytes in argspec
			  collect
			    (loop
				for i from 0 below argbytes
				collect
				  (elt bcode (+ pc0 i))
				finally
				  (setq pc0 (+ pc0 argbytes))))))
		(setq raw-code
		  (nconc raw-code
			 (list (list* pc
				      (instr-mnemonic instr)
				      (pack-raw-args raw-args)))))
		(setq pc pc0)))))
    raw-code))

(defun jvml-disassemble1-wide (bcode pc)
  (let* ((instr (jvml-b2i-wide (elt bcode (1+ pc))))
	 (pc0 (+ pc 2))
	 (raw-args
	  (loop
	      for argbytes in (instr-argspec instr)
	      collect
		(loop
		    for i from 0 below argbytes
		    collect
		      (elt bcode (+ pc0 i))
		    finally
		      (setq pc0 (+ pc0 argbytes))))))
    (values
     (list* pc
	    (instr-mnemonic instr)
	    (pack-raw-args raw-args))
     pc0)))


;;; tableswitch (1)
;;; padding (0-3)
;;; offset-default (4)
;;; offset-low (4)
;;; offset-high (4)
;;; offsets (4 * (high-low+1))

(defun jvml-disassemble1-tableswitch (bcode pc)
  (let* ((npadding (- 3 (mod pc 4)))
	 (padding (get-arg-bytes bcode (+ pc 1) npadding))
	 (offset-default 
	  (apply #'bytes-to-uint4 (get-arg-bytes bcode (+ pc 1 npadding) 4)))
	 (offset-low 
	  (apply #'bytes-to-uint4 (get-arg-bytes bcode (+ pc 1 npadding 4) 4)))
	 (offset-high 
	  (apply #'bytes-to-uint4 (get-arg-bytes bcode (+ pc 1 npadding 8) 4)))
	 (pc0 (+ pc 1 npadding 12))
	 (noffsets (+ (- offset-high offset-low) 1)))
    (declare (ignore padding))
    (values
     (list* pc
	    'i_tableswitch
	    npadding
	    offset-default
	    offset-low
	    offset-high
	    (loop
		for i from 0 below noffsets
		collect
		  (apply #'bytes-to-uint4
			 (get-arg-bytes bcode pc0 4))
		do
		  (setq pc0 (+ pc0 4))))
     pc0)))

;;; lookupswitch (1)
;;; padding (0-3)
;;; offset-default (4)
;;; npairs (4)
;;; match-offset ((4+4) * npairs)

(defun jvml-disassemble1-lookupswitch (bcode pc)
  (let* ((npadding (- 3 (mod pc 4)))
	 (padding (get-arg-bytes bcode (+ pc 1) npadding))
	 (offset-default 
	  (apply #'bytes-to-uint4 (get-arg-bytes bcode (+ pc 1 npadding) 4)))
	 (npairs
	  (apply #'bytes-to-uint4 (get-arg-bytes bcode (+ pc 1 npadding 4) 4)))
	 (pc0 (+ pc 1 npadding 8)))
    (declare (ignore padding))
    (values
     (list* pc
	    'i_lookupswitch
	    npadding
	    offset-default
	    npairs
	    (loop
		for i from 0 below npairs
		collect
		  (list
		   (apply #'bytes-to-uint4 (get-arg-bytes bcode pc0 4))
		   (apply #'bytes-to-uint4 (get-arg-bytes bcode (+ pc0 4) 4)))
		do
		  (setq pc0 (+ pc0 8))))
     pc0)))

(defun pack-raw-args (raw-args)
  (mapcar
   #'(lambda (raw-arg)
       (ecase (length raw-arg)
	 (1 (first raw-arg))
	 (2 (apply #'bytes-to-uint2 raw-arg))
	 (4 (apply #'bytes-to-uint4 raw-arg))))
   raw-args))

(defun get-arg-bytes (bcode pc nbytes)
  (loop
      for i from 0 below nbytes
      collect
	(elt bcode (+ pc i))))

(defun jvml-disasm (ci mname &optional mtype)
  (let ((mi (if mtype
		(find-method-info ci mname mtype)
	      (find-method-info ci mname))))
    (dolist (l (jvml-disassemble mi))
      (pprint l))))
