;;; acl2.asd -- An ad-hoc ASDF system for ACL2 that is solely for
;;; convenience and book-keeping. This will be replaced. Expects to
;;; find ACL2 cloned under QL:*LOCAL-PROJECT-DIRECTORIES*
;;;
(in-package :asdf-user)

(defun find-acl2-installation-directory ()
  (reduce
   (lambda (search-path _)
     (declare (ignore _))
     (when search-path
       (find
        (merge-pathnames "acl" search-path)
        (ql-impl-util:directory-entries search-path)
        :test (lambda (pathname1 pathname2)
                (string-prefix-p
                 (directory-namestring pathname1)
                 (directory-namestring pathname2))))))
   ql:*local-project-directories*
   :initial-value nil
   :from-end t))

(defun build-acl2-fasl ()
  (unless (packagep (find-package "ACL2"))
    (with-current-directory ((find-acl2-installation-directory))
      (let ((*real-standard-output* *standard-output*))
        (with-open-stream (*standard-output* (make-broadcast-stream))
          (when (ql-impl-util:probe-directory #P"./books/quicklisp/bundle")
            (format *real-standard-output*
                "~%WARNING: ACL2 bundles itself with outdated ASDF systems of popular ~
                   ~%         libraries (such as bordaux-threads!). Thus once ACL2 ~
                   ~%         is placed in a place visible to ASDF (and QL), ~
                   ~%         outdated libraries might be loaded! Therefore, it's ~
                   ~%         encouraged to delete the following directory if you ~
                   ~%         have no plans on using it! ~
                   ~%         ~A~%~%"
                (ql-impl-util:probe-directory #P"./books/quicklisp/bundle")))
          (load "init.lisp")
          (format
           *real-standard-output*
           ";   Compiling ACL2 (this may take a few seconds)...~%")
          (funcall (find-symbol "COMPILE-ACL2" "ACL2"))
          (format
           *real-standard-output*
           ";   Loading ACL2 (this will take a few seconds)...~%")
          (funcall (find-symbol "LOAD-ACL2" "ACL2"))
          (format
           *real-standard-output*
           ";   Bootstrapping ACL2's world (this will take 1-2 minutes)... Make some tea.~%")
          (funcall (find-symbol "INITIALIZE-ACL2" "ACL2"))
          (format
           *real-standard-output*
           "; HINT: Use a pre-dumped image next time to speed up the process,~
           ~%       this is until we have a proper ASDF system.~%")))
      nil)))

(defsystem :acl2
  :description "ACL2 ASDF-system signature. (It's a dummy)."
  :perform (load-op (o s) (build-acl2-fasl)))
