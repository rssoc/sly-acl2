;;; slynk-mrepl-acl2 -- ACL2 support for SLYNK-MREPL. The CL side of things.
;;;
;;; What it does:
;;;
;;; This file provides additional support for the SLYNK-MREPL. We
;;; enable the use of ACL2 keyword-commands here, as well as support
;;; for SLYNK-MREPLs's backreference reader syntax. This file is
;;; expected to be loaded after SLYNK-ACL2.lisp. It's an ugly one as
;;; it directly redefines some chunks of SLYNK-MREPL and inlines some
;;; functions from SLYNK-ACL2. This is clearly isn't desirable, so
;;; expect this file to be depreciated in the future.
;;;
(in-package :slynk-mrepl)


;;; NOTE: The easiest way to get back-references working is to
;;; mrepl-get-object-from-history at read-time. Is there a reason why
;;; the backreference-reader doesn't already do this? (I can't think
;;; of one). Keep in sync with SLYNK-MREPL::BACKREFERENCE-READER.
(defun backreference-reader (stream subchar arg)
  "Reads #rfoo:bar into (MREPL-GET-OBJECT-FROM-HISTORY foo bar)."
  (declare (ignore subchar arg))
  (let* ((*readtable*
           (let ((table (copy-readtable nil)))
             (set-macro-character #\: (lambda (&rest args) nil) nil table)
             table))
         (entry-idx
           (progn
             (when (eq #\: (peek-char nil stream nil nil))
               (error 'reader-error
                      :stream stream
                      :format-control "~a found in unexpected place in ~a"
                      :format-arguments `(#\: backreference-reader)))
             (read-preserving-whitespace stream)))
         (value-idx (progn
                      (and (eq #\: (peek-char nil stream nil nil))
                           (read-char stream)
                           (read stream)))))
    (mrepl-get-object-from-history
     entry-idx value-idx)))

;;; NOTE: Keep in sync with SLYNK-MREPL::MREPL-EVAL-1,
;;; SLYNK-ACL2::READ-STRING-AS-ACL2, and
;;; SLYNK-ACL2::EVAL-IN-ACL2.
(defun mrepl-eval-1 (repl string)
  "In REPL's environment, READ and EVAL forms in STRING."
  (with-sly-interrupts
    (with-listener-bindings repl
      (prog1
          (with-retry-restart (:msg "Retry SLY mREPL evaluation request.")
            (with-input-from-string (in string)
              (when (and (slynk-acl2::acl2-package? *package*)
                         (eq (peek-char t in) #\:))
                (slynk-acl2::maybe-initialize-acl2)
                acl2::(with-suppression
                          (ld-fn
                           `((standard-oi . slynk-mrepl::(,(cl:read in nil)
                                                          ,(cl:read in nil)))
                             (ld-prompt . nil)
                             (ld-verbose . nil)
                             (ld-error-triples . t)
                             (ld-post-eval-print . nil)
                             (ld-error-action . :error))
                           *the-live-state*
                           nil)))
              (loop with values
                    for form =
                             (let ((*readtable* (let ((table
                                                        (copy-readtable
                                                         (if (slynk-acl2::acl2-package? *package)
                                                             acl2::*acl2-readtable*
                                                             *readtable*))))
                                                  (if *backreference-character*
                                                      (set-dispatch-macro-character
                                                       #\#
                                                       *backreference-character*
                                                       #'backreference-reader table))
                                                  table)))
                               (cl:read in nil in))
                    until (eq form in)
                    do (let ((- form))
                         (setq values (multiple-value-list
                                       (eval
                                        (saving-listener-bindings repl
                                          (setq +++ ++ ++ + + form))))))
                    finally
                       (return values))))
        (dolist (special-sym '(*package* *default-pathname-defaults*))
          (setf (cdr (assoc special-sym (slot-value repl 'slynk::env)))
                (symbol-value special-sym)))))))
