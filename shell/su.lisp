;;(defpackage :shcl/shell/su (:use :cl)
;;            (:import-from :shcl/core/commnad
;;                          :define-builtin))
;;
;;(in-package :shcl/shell/su)
;;
;;(defun option-p (str)
;;    (and (plusp (length str))
;;                (char= (char str 0) #\-)))
;;
;;(defun parse-su-args (args)
;;  (let ((command-name (pop args))
;;        (options '())
;;        user
;;        group)
;;    (declare (ignore command-name))
;;    (do ((arg (pop args) (pop args)))
;;        ((not (cdr args)) nil)
;;      (if (option-p arg)
;;          (cond 
;;            ((equal "-c" arg) 
;;             (push (cons :command (pop args)) options))
;;            (t (error 'command-error :message "unsupported option provided")))
;;          (if user
;;              (if (not group) 
;;                  (setq group args)
;;                  (error 'command-error :message "invalid arguments"))
;;              (setq user arg))))
;;  (values user group options)))
;;
;;(define-builtin (builtin-su "su") (&whole whole &rest args)
;;  "Change user id, among other things"
;;
;;  (multiple-value-bind (user group options)
;;
;;      
;;      
;;      ))