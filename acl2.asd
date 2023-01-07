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
    (with-current-directory (#.(find-acl2-installation-directory))
      (let ((*real-standard-output* *standard-output*))
        (with-open-stream (*standard-output* (make-broadcast-stream))
          (format
           *real-standard-output*
           "; Loading \"ACL2\"~%")
          (load "init.lisp")
          (format
           *real-standard-output*
           ";   Compiling ACL2 (this will take a few seconds)...~%")
          (funcall (find-symbol "COMPILE-ACL2" "ACL2"))
          (format
           *real-standard-output*
           ";   Loading ACL2 (this will take a few seconds)...~%")
          (funcall (find-symbol "LOAD-ACL2" "ACL2"))
          (format
           *real-standard-output*
           ";   Initializing ACL2 (this may take 1-2 minutes)... Make some tea.~%")
          (funcall (find-symbol "INITIALIZE-ACL2" "ACL2"))
          (format
           *real-standard-output*
           "; HINT: Use a pre-dumped image next time to speed up the process~%")
          (format
           *real-standard-output*
           ";       (until we have a proper ASDF system definition).~%")))
      nil)))

(defsystem :acl2
  :class :precompiled-system
  :description "ACL2 ASDF-system signature. (It's a dummy)."
  :fasl #.(build-acl2-fasl))
