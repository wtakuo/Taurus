;;; -*- Mode: Common-Lisp; Package: Taurus; -*-

(in-package :taurus)

(defun compile-java-file (file)
  #+EXCL
  (let ((cmd (format nil "LC_ALL=C javac ~A" file)))
    (format t "~%$ ~A" cmd)
    (run-shell-command cmd))
  #-EXCL
  (error "compile-java-file: not available in this port.")
  )
