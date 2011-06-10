;;; -*- Mode: Common-Lisp; Package: Taurus; -*-
;;;
;;; jvml-instructions.cl
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

;;; (bytecode mnemonic argsize argtype stackspec)
;;;
;;; argtypes
;;;    n : immediate value
;;;    i : constant pool index
;;;    v : local variable index
;;;    x : array type specifier
;;;    o : jump address offset
;;;
;;; types
;;;   :ref :ra
;;;   :int :long :float :double
;;;   :int_float_or_ref :long_or_double
;;;   :c1 :c2 :any

(defconstant *jvml-instructions*
    '((000 i_nop             ()      ()      (() ()))
      (001 i_aconst_null     ()      ()      (() (:ref)))
      (002 i_iconst_m1       ()      ()      (() (:int)))
      (003 i_iconst_0        ()      ()      (() (:int)))
      (004 i_iconst_1        ()      ()      (() (:int)))
      (005 i_iconst_2        ()      ()      (() (:int)))
      (006 i_iconst_3        ()      ()      (() (:int)))
      (007 i_iconst_4        ()      ()      (() (:int)))
      (008 i_iconst_5        ()      ()      (() (:int)))
      (009 i_lconst_0        ()      ()      (() (:int)))
      (010 i_lconst_1        ()      ()      (() (:int)))
      (011 i_fconst_0        ()      ()      (() (:float)))
      (012 i_fconst_1        ()      ()      (() (:float)))
      (013 i_fconst_2        ()      ()      (() (:float)))
      (014 i_dconst_0        ()      ()      (() (:double)))
      (015 i_dconst_1        ()      ()      (() (:double)))
      (016 i_bipush          (1)     (n)     (() (:int)))
      (017 i_sipush          (2)     (n)     (() (:int)))
      (018 i_ldc             (1)     (i)     (() (:int_float_or_ref)))
      (019 i_ldc_w           (2)     (i)     (() (:int_float_or_ref)))
      (020 i_ldc2_w          (2)     (i)     (() (:long_or_double)))
      (021 i_iload           (1)     (v)     (() (:int)))
      (022 i_lload           (1)     (v)     (() (:long)))
      (023 i_fload           (1)     (v)     (() (:float)))
      (024 i_dload           (1)     (v)     (() (:double)))
      (025 i_aload           (1)     (v)     (() (:ref)))
      (026 i_iload_0         ()      ()      (() (:int)))
      (027 i_iload_1         ()      ()      (() (:int)))
      (028 i_iload_2         ()      ()      (() (:int)))
      (029 i_iload_3         ()      ()      (() (:int)))
      (030 i_lload_0         ()      ()      (() (:long)))
      (031 i_lload_1         ()      ()      (() (:long)))
      (032 i_lload_2         ()      ()      (() (:long)))
      (033 i_lload_3         ()      ()      (() (:long)))
      (034 i_fload_0         ()      ()      (() (:float)))
      (035 i_fload_1         ()      ()      (() (:float)))
      (036 i_fload_2         ()      ()      (() (:float)))
      (037 i_fload_3         ()      ()      (() (:float)))
      (038 i_dload_0         ()      ()      (() (:double)))
      (039 i_dload_1         ()      ()      (() (:double)))
      (040 i_dload_2         ()      ()      (() (:double)))
      (041 i_dload_3         ()      ()      (() (:double)))
      (042 i_aload_0         ()      ()      (() (:ref)))
      (043 i_aload_1         ()      ()      (() (:ref)))
      (044 i_aload_2         ()      ()      (() (:ref)))
      (045 i_aload_3         ()      ()      (() (:ref)))
      (046 i_iaload          ()      ()      ((:int :ref) (:int)))
      (047 i_laload          ()      ()      ((:int :ref) (:long)))
      (048 i_faload          ()      ()      ((:int :ref) (:float)))
      (049 i_daload          ()      ()      ((:int :ref) (:double)))
      (050 i_aaload          ()      ()      ((:int :ref) (:ref)))
      (051 i_baload          ()      ()      ((:int :ref) (:int)))
      (052 i_caload          ()      ()      ((:int :ref) (:int)))
      (053 i_saload          ()      ()      ((:int :ref) (:int)))
      (054 i_istore          (1)     (v)     ((:int) ()))
      (055 i_lstore          (1)     (v)     ((:long) ()))
      (056 i_fstore          (1)     (v)     ((:float) ()))
      (057 i_dstore          (1)     (v)     ((:double) ()))
      (058 i_astore          (1)     (v)     ((:ref) ()))
      (059 i_istore_0        ()      ()      ((:int) ()))
      (060 i_istore_1        ()      ()      ((:int) ()))
      (061 i_istore_2        ()      ()      ((:int) ()))
      (062 i_istore_3        ()      ()      ((:int) ()))
      (063 i_lstore_0        ()      ()      ((:long) ()))
      (064 i_lstore_1        ()      ()      ((:long) ()))
      (065 i_lstore_2        ()      ()      ((:long) ()))
      (066 i_lstore_3        ()      ()      ((:long) ()))
      (067 i_fstore_0        ()      ()      ((:float) ()))
      (068 i_fstore_1        ()      ()      ((:float) ()))
      (069 i_fstore_2        ()      ()      ((:float) ()))
      (070 i_fstore_3        ()      ()      ((:float) ()))
      (071 i_dstore_0        ()      ()      ((:double) ()))
      (072 i_dstore_1        ()      ()      ((:double) ()))
      (073 i_dstore_2        ()      ()      ((:double) ()))
      (074 i_dstore_3        ()      ()      ((:double) ()))
      (075 i_astore_0        ()      ()      ((:ref) ()))
      (076 i_astore_1        ()      ()      ((:ref) ()))
      (077 i_astore_2        ()      ()      ((:ref) ()))
      (078 i_astore_3        ()      ()      ((:ref) ()))
      (079 i_iastore         ()      ()      ((:int :int :ref) ()))
      (080 i_lastore         ()      ()      ((:long :int :ref) ()))
      (081 i_fastore         ()      ()      ((:float :int :ref) ()))
      (082 i_dastore         ()      ()      ((:double :int :ref) ()))
      (083 i_aastore         ()      ()      ((:ref :int :ref) ()))
      (084 i_bastore         ()      ()      ((:int :int :ref) ()))
      (085 i_castore         ()      ()      ((:int :int :ref) ()))
      (086 i_sastore         ()      ()      ((:int :int :ref) ()))
      (087 i_pop             ()      ()      ((:c1) ()))
      (088 i_pop2            ()      ()      (:or ((:c2) ())
 				                  ((:c1 :c1) ())))
      (089 i_dup             ()      ()      ((:c1) (:c1 :c1)))
      (090 i_dup_x1          ()      ()      ((:c1 :c1) (:c1 :c1 :c1)))
      (091 i_dup_x2          ()      ()      (:or ((:c1 :c1 :c1) 
						   (:c1 :c1 :c1 :c1))
				                  ((:c1 :c2) (:c1 :c2 :c1))))
      (092 i_dup2            ()      ()      (:or ((:c1 :c1) (:c1 :c1 :c1 :c1))
					          ((:c2) (:c2 :c2))))
      (093 i_dup2_x1         ()      ()      (:or ((:c1 :c1 :c1) 
						   (:c1 :c1 :c1 :c1 :c1))
					          ((:c2 :c1) (:c2 :c1 :c2))))
      (094 i_dup2_x2         ()      ()      (:or ((:c1 :c1 :c1 :c1) 
						   (:c1 :c1 :c1 :c1 :c1 :c1))
					          ((:c2 :c1 :c1) 
						   (:c2 :c1 :c1 :c2))
					          ((:c1 :c1 :c2) 
						   (:c1 :c1 :c2 :c1 :c1))
					          ((:c2 :c2) 
						   (:c2 :c2 :c2))))
      (095 i_swap            ()      ()      ((:c1 :c1) (:c1 :c1)))
      (096 i_iadd            ()      ()      ((:int :int) (:int)))
      (097 i_ladd            ()      ()      ((:long :long) (:long)))
      (098 i_fadd            ()      ()      ((:float :float) (:float)))
      (099 i_dadd            ()      ()      ((:double :double) (:double)))
      (100 i_isub            ()      ()      ((:int :int) (:int)))
      (101 i_lsub            ()      ()      ((:long :long) (:long)))
      (102 i_fsub            ()      ()      ((:float :float) (:float)))
      (103 i_dsub            ()      ()      ((:double :double) (:double)))
      (104 i_imul            ()      ()      ((:int :int) (:int)))
      (105 i_lmul            ()      ()      ((:long :long) (:long)))
      (106 i_fmul            ()      ()      ((:float :float) (:float)))
      (107 i_dmul            ()      ()      ((:double :double) (:double)))
      (108 i_idiv            ()      ()      ((:int :int) (:int)))
      (109 i_ldiv            ()      ()      ((:long :long) (:long)))
      (110 i_fdiv            ()      ()      ((:float :float) (:float)))
      (111 i_ddiv            ()      ()      ((:double :double) (:double)))
      (112 i_irem            ()      ()      ((:int :int) (:int)))
      (113 i_lrem            ()      ()      ((:long :long) (:long)))
      (114 i_frem            ()      ()      ((:float :float) (:float)))
      (115 i_drem            ()      ()      ((:double :double) (:double)))
      (116 i_ineg            ()      ()      ((:int) (:int)))
      (117 i_lneg            ()      ()      ((:long) (:long)))
      (118 i_fneg            ()      ()      ((:float) (:float)))
      (119 i_dneg            ()      ()      ((:double) (:double)))
      (120 i_ishl            ()      ()      ((:int :int) (:int)))
      (121 i_lshl            ()      ()      ((:int :long) (:long)))
      (122 i_ishr            ()      ()      ((:int :int) (:int)))
      (123 i_lshr            ()      ()      ((:int :long) (:long)))
      (124 i_iushr           ()      ()      ((:int :int) (:int)))
      (125 i_lushr           ()      ()      ((:int :long) (:long)))
      (126 i_iand            ()      ()      ((:int :int) (:int)))
      (127 i_land            ()      ()      ((:long :long) (:long)))
      (128 i_ior             ()      ()      ((:int :int) (:int)))
      (129 i_lor             ()      ()      ((:long :long) (:long)))
      (130 i_ixor            ()      ()      ((:int :int) (:int)))
      (131 i_lxor            ()      ()      ((:long :long) (:long)))
      (132 i_iinc            (1 1)   (v n)   (() ()))
      (133 i_i2l             ()      ()      ((:int) (:long)))
      (134 i_i2f             ()      ()      ((:int) (:float)))
      (135 i_i2d             ()      ()      ((:int) (:double)))
      (136 i_l2i             ()      ()      ((:long) (:int)))
      (137 i_l2f             ()      ()      ((:long) (:float)))
      (138 i_l2d             ()      ()      ((:long) (:double)))
      (139 i_f2i             ()      ()      ((:float) (:int)))
      (140 i_f2l             ()      ()      ((:float) (:long)))
      (141 i_f2d             ()      ()      ((:float) (:double)))
      (142 i_d2i             ()      ()      ((:double) (:int)))
      (143 i_d2l             ()      ()      ((:double) (:long)))
      (144 i_d2f             ()      ()      ((:double) (:float)))
      (145 i_i2b             ()      ()      ((:int) (:int)))
      (146 i_i2c             ()      ()      ((:int) (:int)))
      (147 i_i2s             ()      ()      ((:int) (:int)))
      (148 i_lcmp            ()      ()      ((:long :long) (:int)))
      (149 i_fcmpl           ()      ()      ((:float :float) (:int)))
      (150 i_fcmpg           ()      ()      ((:float :float) (:int)))
      (151 i_dcmpl           ()      ()      ((:double :double) (:int)))
      (152 i_dcmpg           ()      ()      ((:double :double) (:int)))
      (153 i_ifeq            (2)     (o)     ((:int) ()))
      (154 i_ifne            (2)     (o)     ((:int) ()))
      (155 i_iflt            (2)     (o)     ((:int) ()))
      (156 i_ifge            (2)     (o)     ((:int) ()))
      (157 i_ifgt            (2)     (o)     ((:int) ()))
      (158 i_ifle            (2)     (o)     ((:int) ()))
      (159 i_if_icmpeq       (2)     (o)     ((:int :int) ()))
      (160 i_if_icmpne       (2)     (o)     ((:int :int) ()))
      (161 i_if_icmplt       (2)     (o)     ((:int :int) ()))
      (162 i_if_icmpge       (2)     (o)     ((:int :int) ()))
      (163 i_if_icmpgt       (2)     (o)     ((:int :int) ()))
      (164 i_if_icmple       (2)     (o)     ((:int :int) ()))
      (165 i_if_acmpeq       (2)     (o)     ((:int :int) ()))
      (166 i_if_acmpne       (2)     (o)     ((:int :int) ()))
      (167 i_goto            (2)     (o)     (() ()))
      (168 i_jsr             (2)     (o)     (() (:ra)))
      (169 i_ret             (1)     (v)     (() ()))
      (170 i_tableswitch     :special)
      (171 i_lookupswitch    :special)
      (172 i_ireturn         ()      ()      ((:int) :empty))
      (173 i_lreturn         ()      ()      ((:long) :empty))
      (174 i_freturn         ()      ()      ((:float) :empty))
      (175 i_dreturn         ()      ()      ((:double) :empty))
      (176 i_areturn         ()      ()      ((:ref) :empty))
      (177 i_return          ()      ()      (() :empty))
      (178 i_getstatic       (2)     (i)     (() (:any)))
      (179 i_putstatic       (2)     (i)     ((:any) ()))
      (180 i_getfield        (2)     (i)     ((:ref) (:any)))
      (181 i_putfield        (2)     (i)     ((:any :ref) ()))
      (182 i_invokevirtual   (2)     (i)     (:args_ref :result))
      (183 i_invokespecial   (2)     (i)     (:args_ref :result))
      (184 i_invokestatic    (2)     (i)     (:args_ref :result))
      (185 i_invokeinterface (2 1 1) (i n n) (:args_ref :result))
      (187 i_new             (2)     (i)     (() (:ref)))
      (188 i_newarray        (1)     (x)     ((:int) (:ref)))
      (189 i_anewarray       (2)     (i)     ((:int) (:ref)))
      (190 i_arraylength     ()      ()      ((:ref) (:int)))
      (191 i_athrow          ()      ()      ((:ref) :empty (:ref)))
      (192 i_checkcast       (2)     (i)     ((:ref) (:ref)))
      (193 i_instanceof      (2)     (i)     ((:ref) (:int)))
      (194 i_monitorenter    ()      ()      ((:ref) ()))
      (195 i_monitorexit     ()      ()      ((:ref) ()))
      (196 i_wide            :special)
      (197 i_multianewarray  (2 1)   (i n)   (:counts (:ref)))
      (198 i_ifnull          (2)     (o)     ((:ref) ()))
      (199 i_ifnonnull       (2)     (o)     ((:ref) ()))
      (200 i_goto_w          (4)     (w)     (() ()))
      (201 i_jsr_w           (4)     (w)     (() ()))
      ;; VM dependent instructions
      (202 i_breakpoint      ()      ()      (() ()))
      (254 i_impdep1         ()      ()      (() ()))
      (255 i_impdep2         ()      ()      (() ()))
      ))

(defconstant *jvml-wide-instructions*
    '(
      (021 i_wide_iload  "wide iload"  (2)    (v)   (() (:int)))
      (022 i_wide_lload  "wide lload"  (2)    (v)   (() (:long)))
      (023 i_wide_fload  "wide fload"  (2)    (v)   (() (:float)))
      (024 i_wide_dload  "wide dload"  (2)    (v)   (() (:double)))
      (025 i_wide_aload  "wide aload"  (2)    (v)   (() (:ref)))
      (054 i_wide_istore "wide istore" (2)    (v)   ((:int) ()))
      (055 i_wide_lstore "wide lstore" (2)    (v)   ((:long) ()))
      (056 i_wide_fstore "wide fstore" (2)    (v)   ((:float) ()))
      (057 i_wide_dstore "wide dstore" (2)    (v)   ((:double) ()))
      (058 i_wide_astore "wide astore" (2)    (v)   ((:ref) ()))
      (169 i_wide_ret    "wide ret"    (2)    (v)   (() ()))
      (132 i_wide_iinc   "wide iinc"   (2 2)  (v n) (() ()))
      ))

(defconstant *jvml-branch-instructions*
    '(i_ifeq
      i_ifne
      i_iflt
      i_ifge
      i_ifgt
      i_ifle
      i_if_icmpeq
      i_if_icmpne
      i_if_icmplt
      i_if_icmpge
      i_if_icmpgt
      i_if_icmple
      i_if_acmpeq
      i_if_acmpne
      i_ifnull
      i_ifnonnull
      ))

(defconstant *jvml-jump-instructions*
    '(i_goto
      i_ret
      i_tableswitch
      i_lookupswitch
      i_goto_w
      ))

(defconstant *jvml-tail-instructions*
    '(i_ireturn
      i_lreturn
      i_freturn
      i_dreturn
      i_areturn
      i_return
      i_athrow
      ))

(defconstant *jvml-block-tail-instructions*
    (append *jvml-branch-instructions*
	    *jvml-jump-instructions*
	    *jvml-tail-instructions*))

(defvar *jvml-instruction-array*)
(defvar *jvml-instruction-hash*)

(defun setup-jvml-instruction-tables ()
  (let ((a (make-array 256))
	(h (make-hash-table)))
    (dotimes (i 256)
      (setf (elt a i) :unused))
    (dolist (instr *jvml-instructions*)
      (let ((entry 
	     (list* (car instr)
		    (cadr instr)
		    (subseq (string-downcase (symbol-name (cadr instr))) 2)
		    (cddr instr))))
	(setf (elt a (car instr)) entry)
	(setf (gethash (cadr instr) h) entry)))
    (dolist (instr *jvml-wide-instructions*)
      (let ((entry
	     (cons (list 196 (car instr))
		   (cdr instr))))
	(setf (gethash (cadr instr) h) entry)))
    (setq *jvml-instruction-array* a)
    (setq *jvml-instruction-hash* h)))

(defun jvml-b2i (bytecode)
  (let ((instr (elt *jvml-instruction-array* bytecode)))
    (if (eq instr :unused)
	(error "Unused bytecode ~S" bytecode)
      instr)))

(defun jvml-b2i-wide (bytecode)
  (let ((instr (assoc bytecode *jvml-wide-instructions*)))
    (if (null instr)
	(error "Invalid wide instruction ~S" bytecode)
      instr)))

(defun jvml-m2i (mnemonic)
  (let ((instr (gethash mnemonic *jvml-instruction-hash*)))
    (if (null instr)
	(error "Unknown mnemonic ~S" mnemonic)
      instr)))

;;; instruction accessors
;;; (bytecode mnemonic name args stackspec)

(defun instr-bytecode (instr) (first instr))
(defun instr-mnemonic (instr) (second instr))
(defun instr-name (instr) (third instr))
(defun instr-argspec (instr) (fourth instr))
(defun instr-argtype (instr) (fifth instr))
(defun instr-stackspec (instr) (sixth instr))


(eval-when (load eval)
  (setup-jvml-instruction-tables)
  )
