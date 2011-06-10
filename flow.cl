;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; flow.cl
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

;;; compute control flow relation on blocks

(defun compute-cfr (code)
  (compute-cfr-fixpoint (compute-cfr1 code)))

(defun compute-cfr1 (code)
  (match code
    (((_LBL . _BLK) . _REST)
     (plet ((_MNEMONIC . _ARGS) (car (last _BLK)))
       (cond ((member _MNEMONIC *jvml-tail-instructions*)
	      (compute-cfr _REST))
	     ((member _MNEMONIC *jvml-jump-instructions*)
	      (ecase _MNEMONIC
		((i_goto i_goto_w)
		 (list* (list _LBL (car _ARGS))
			(compute-cfr1 _REST)))
		(i_ret
		 (compute-cfr1 _REST))
		;; args = (default l h b1 ...)
		(i_tableswitch
		 (cons
		  (cons _LBL
			(remove-duplicates
			 (cons (car _ARGS) (cdddr _ARGS))))
		  (compute-cfr1 _REST)))
		;; args = (default n (v1 b1) ...)
		(i_lookupswitch
		 (cons
		  (cons _LBL
			(remove-duplicates 
			 (cons (car _ARGS) (mapcar #'cadr (cddr _ARGS)))))
		  (compute-cfr1 _REST)))))
	     ((member _MNEMONIC *jvml-branch-instructions*)
	      (cons (list _LBL (car _ARGS) (caar _REST))
		    (compute-cfr1 _REST)))
	     (t
	      (cons (list _LBL (caar _REST))
		    (compute-cfr1 _REST))))))
     (() ())))

(defun compute-cfr-fixpoint (rs &aux rs0)
  (loop
    (setq rs0 (copy-tree rs))
    (dolist (r rs0)
      (let ((r1 (assoc (car r) rs)))
	(dolist (x (cdr r1))
	  (dolist (y (cdr (assoc x rs)))
	    (unless (member y r)
	      (nconc r (list y)))))))
    (if (equal rs0 rs)
	(return-from compute-cfr-fixpoint rs0)
      (setq rs rs0))))
