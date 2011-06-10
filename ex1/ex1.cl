;;; -*- Mode: Common-Lisp; Package: Taurus; -*-

(in-package :taurus)

(defparameter *target-classes*
    '("MailingAgent" "MailingAgent$1"))

(defparameter *target-method-specs*
    '(
      ;(:method "Mailer" "getUserName" "()Ljava/lang/String;")
      ;(:method "Mailer" "getCalendar" "()LCalendar;")
      (:method "Mailer" "doQuery" "(LAction;)V")
      (:method "Mailer" "sendMail" 
       "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V")))

(defun ex1 ()
  (compile-java-file "MailingAgent.java")
  (let ((observer-type-name "MailerPolicyObserver")
	(observer-variable-name "__OBSERVER__")
	(observer-setter-name "setObserver"))
    (dolist (cf (mapcar #'(lambda (cname) (format nil "~A.class" cname))
			*target-classes*))
      (format t "~%~A" cf)
      (let ((ci (load-class cf)))
	(set-invokevirtual-observation ci
				       *target-method-specs*
				       observer-type-name
				       observer-variable-name)
	(save-class ci cf)))))
    
