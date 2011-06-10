;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; jvml-assembler.cl
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

;;; blockified code
;;;   code ::= (block ...)
;;;   block ::= (label line ...)
;;;   line ::= (mnemonic arg ...)
;;; exception table
;;;   exc-tbl ::= ((label label label type) ...)

(defun jvml-assemble (code exc-tbl cp)
  (let ((lbltbl (list :dummy))
	(pc 0)
	(raw-code0 nil)
	(raw-code1 nil)
	(exc-tbl0 nil))
    ;; 1. calculate line addresses
    (dolist (line (unblockify code))
      (if (symbolp line)
	  (intern-label lbltbl line pc)
	(let* ((mnemonic (car line))
	       (args (cdr line))
	       (instr (jvml-m2i mnemonic))
	       (instr-size 
		(if (listp (instr-bytecode instr)) 2 1))
	       (argspec (instr-argspec instr)))
	  (case mnemonic
	    (i_tableswitch
	     (let ((npadding (- 3 (mod pc 4))))
	       (setq raw-code0
		 (append raw-code0
			 (list (list* pc mnemonic npadding args))))
	       (setq pc (+ pc instr-size npadding (* 4 (length args))))))
	    (i_lookupswitch
	     (let ((npadding (- 3 (mod pc 4))))
	       (setq raw-code0
		 (append raw-code0
			 (list (list* pc mnemonic npadding args))))
	       (setq pc
		 (+ pc instr-size npadding 8 (* 8 (length (cddr args)))))))
	    (t
	     (setq raw-code0
	       (append raw-code0
		       (list (list* pc mnemonic args))))
	     (setq pc (apply #'+ pc instr-size argspec)))))))
    ;; 2. fix arguemnts
    (dolist (line raw-code0)
      (let* ((addr (first line))
	     (mnemonic (second line))
	     (args (cddr line)))
	(case mnemonic
	  ;; args = (npadding default low high offset1 ...)
	  (i_tableswitch
	   (setq raw-code1
	     (append raw-code1
		     (list
		      (list*
		       addr
		       mnemonic
		       (first args)	; npadding
		       (calculate-offset32 addr (second args) lbltbl)
		       (third args)
		       (fourth args)
		       (mapcar #'(lambda (label)
				   (calculate-offset32 addr
						       label
						       lbltbl))
			       (cddddr args)))))))
	  ;; args = (npadding default npairs (v o) ...)
	  (i_lookupswitch
	   (setq raw-code1
	     (append raw-code1
		     (list
		      (list*
		       addr
		       mnemonic
		       (first args)	; npadding
		       (calculate-offset32 addr (second args) lbltbl)
		       (third args)
		       (mapcar #'(lambda (p)
				   (list (first p)
					 (calculate-offset32 addr
							     (second p)
							     lbltbl)))
			       (cdddr args)))))))
	  (t
	   (let ((argtypes (instr-argtype (jvml-m2i mnemonic))))
	     (setq raw-code1
	       (append raw-code1
		       (list 
			(list* 
			 addr
			 mnemonic
			 (mapcar
			  #'(lambda (arg argtype)
			      (ecase argtype
				(n (second arg))
				(i (apply #'intern-cp-element cp arg))
				(v (second arg))
				(x (get-array-type-code arg))
				(o (calculate-offset16 addr arg lbltbl))
				(w (calculate-offset32 addr arg lbltbl))))
			  args
			  argtypes))))))))))
    ;; 3 fix exception table
    (setq exc-tbl0
      (mapcar #'(lambda (e)
		  (list (find-address lbltbl (first e))
			(find-address lbltbl (second e))
			(find-address lbltbl (third e))
			(if (eq (fourth e) :any)
			    0
			  (apply #'intern-cp-element cp (fourth e)))))
	      exc-tbl))

    (values (apply #'vector (jvml-assemble1 raw-code1) )
	    exc-tbl0)))

;;; unblockified code
;;;   code ::= (line ...)
;;;   line ::= label | (mnemonic arg ...)

(defun unblockify (code)
  (if (null code)
      nil
    (append (car code) (unblockify (cdr code)))))

(defun calculate-offset16 (addr label lbltbl)
  (int2-to-uint2
   (- (find-address lbltbl label) addr)))

(defun calculate-offset32 (addr label lbltbl)
  (int4-to-uint4
   (- (find-address lbltbl label) addr)))

(defun find-address (lbltbl label)
  (let ((a (assoc label (cdr lbltbl))))
    (if a
	(cdr a)
      (error "unknwon label ~S" label))))

(defun intern-label (lbltbl label addr)
  (let ((a (assoc label (cdr lbltbl))))
    (if (null a)
	(nconc lbltbl (list (cons label addr))))))

;;; raw-code ::= (raw-line ...)
;;; raw-line ::= (addr mnemonic raw-arg ...)

(defun jvml-assemble1 (raw-code)
  (loop
      for line in raw-code
      append
	(jvml-assemble1-line line)))

(defun jvml-assemble1-line (line)
  (let* ((instr (jvml-m2i (second line)))
	 (args (cddr line))
	 (bytecode (instr-bytecode instr))
	 (argspec (instr-argspec instr)))
    (case (instr-mnemonic instr)
      ;; args = (npadding default low high offset1 ...)
      (i_tableswitch
       (cons bytecode
	     (append (ecase (first args)
		       (0 nil)
		       (1 (list 0))
		       (2 (list 0 0))
		       (3 (list 0 0 0)))
		     (mapcan #'uint4-to-byte-list (cdr args)))))
      ;; args = (npadding default npairs (value1 offset1) ...)
      (i_lookupswitch
       (cons bytecode
	     (append (ecase (first args)
		       (0 nil)
		       (1 (list 0))
		       (2 (list 0 0))
		       (3 (list 0 0 0)))
		     (append
		      (uint4-to-byte-list (second args))
		      (uint4-to-byte-list (third args)))
		     (mapcan
		      #'(lambda (p)
			  (append
			   (uint4-to-byte-list (first p))
			   (uint4-to-byte-list (second p))))
		      (cdddr args)))))
      (t
       (append (if (listp bytecode) 
		   bytecode   ; wide instructions
		 (list bytecode))
	       (unpack-args args argspec))))))

(defun unpack-args (args argspec)
  (mapcan #'(lambda (arg nbytes)
	      (ecase nbytes
		(1 (list arg))
		(2 (uint2-to-byte-list arg))
		(4 (uint4-to-byte-list arg))))
	  args
	  argspec))
