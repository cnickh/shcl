(defpackage 
    :shcl/core/contain (:use :cl)
    (:export
     :with-gid
     :with-uid
     :with-root))

(in-package :shcl/core/contain)

;;Some quick and dirty shcl additions for providing more isolation to spawned processes.
;;TODO make builtin commands honor what is set here.

(defvar *root* nil)
(defvar *uid* nil)
(defvar *gid* nil)

(defmacro with-uid (uid &rest body)
  `(let ((*uid* ,uid))
     (declare (type fixnum *uid*))
     ,@body))

(defmacro with-gid (gid &rest body)
  `(let ((*gid* ,gid))
     (declare (type fixnum *gid*))
     ,@body))

(defmacro with-root (root &rest body)
  `(let ((*root* ,root))
     ;;TODO assert valid root path, should be pretty robust as a lot can go wrong with a bad chroot
     ,@body))