;;; slynk-acl2.lisp -- ACL2 support for SLYNK. The CL side of things.
;;;
;;; How it works:
;;;
;;; Evaluating this file has the side-effect of shadowing EVAL, READ,
;;; and READ-FROM-STRING, in the :SLYNK and :SLYNK/MREPL
;;; packages. These functions behave identically to their COMMON-LISP
;;; counterparts in, until we need to re-direct an ACL2 expression
;;; into the carefully set-up evaluation environment that it expects
;;; (and the entry point into that is provided by ACL2::LD-FN,
;;; thanks!).
;;;
;;; We determine at read-time whether a form should be interpreted by
;;; the host lisp, or by ACL2 (see `acl2-expression-p' for the
;;; criteria that determines this). If an ACL2 form was read, a
;;; lisp-object is read in w.r.t. ACL2::*ACL2-READTABLE*, and the read
;;; function returns that lisp-object.
;;;
;;; Then, if EVAL tries to evaluate an ACL2 expression, it's handed
;;; off to ACL2::LD-FN for evaluation, and returns the result of that
;;; expression in the ACL2 environment. The returned values are not
;;; specially post-processed for presentation, unlike in the normal
;;; ACL2::LP, thus while it may appear that a function has returned
;;; more than expected, be assured it is the true return value of that
;;; function.
;;;
(defpackage :slynk-acl2
  (:use :cl)
  (:shadow :eval :read :read-from-string))

(in-package :slynk-acl2)


;; NOTE: We only respect these if they're at the top-level. This works
;; well for my purposes, but should I one day figure out (or need to) carve
;; them out of a greater ACL2 expression?
(defvar *meta-symbols*
  '("IN-PACKAGE")
  "There are certain COMMON-LISP symbols we'd always like to be processed
by the host lisp; no matter the context.")

(defun meta-symbol? (symbol)
  "Test whether SYMBOL is in a meta-symbol. See `*meta-symbols*'."
  (member (symbol-name symbol)
          *meta-symbols*
          :test #'string-equal))


;;; TODO: Figure out if I should be using ACL2::STATE or
;;; ACL2::*THE-LIVE-STATE*. (There's also ACL2::STATE-STATE)!
(defun list-acl2-packages ()
  "Returns a list of packages defined in ACL2's namespace."
  (mapcar
   #'find-package
   (set-difference
    acl2::(strip-non-hidden-package-names
           (known-package-alist *the-live-state*))
    ;; It's not the COMMON-LISP we know and... love? Like KEYWORD,
    ;; it's a pseudo-package that we must give special treatment to.
    '("COMMON-LISP" "KEYWORD")
    :test #'string-equal)))

(defun acl2-package? (package)
  "Test whether PACKAGE is defined in ACL2's namespace."
  (and (member package (list-acl2-packages))
       t))

(defun acl2-symbol? (symbol)
  "Test whether SYMBOL is trying to express itself in ACL2."
  (unless (meta-symbol? symbol)
    (or (acl2-package? (symbol-package symbol))
        ;; Although it appears as a COMMON-LISP symbol to us, in
        ;; reality they are special to ACL2. This fake appearance is
        ;; somewhat annoying as it can cause some unexpected
        ;; results. Later, I will want to fix this through the reader
        ;; (if a COMMON-LISP symbol was explicitly qualified as an
        ;; ACL2 symbol, keep it as such).
        (and (acl2-package? *package*)
             (eq (symbol-package symbol)
                 (find-package "COMMON-LISP"))))))

;;; TODO: This is basic... Or is it? Do I really want to start mixing
;;; CL and ACL2 expressions together? Would this functionality be left
;;; in better taste as another package?
(defun acl2-expression? (expression)
  "Test whether EXPRESSION is trying to express itself in ACL2."
  (cond ((symbolp expression)
         (acl2-symbol? expression))
        ((consp expression)
         (acl2-expression? (car expression)))
        ;; NOTE: Slynk (I'm assuming) sends a very weird lisp-object
        ;; here. Just trying to look at it causes a memory fault (I'm
        ;; on SBCL 2.2.10).
        (t nil)))

(defun acl2-last-input-expression ()
  "Returns the last input expression sent to ACL2."
  acl2::(ld-history-entry-input
         (first (ld-history *the-live-state*))))

(defun ensure-list (obj)
  "Coerce OBJ into a list by wrapping it in one (if necessary)."
  (if (listp obj)
      obj
      (list obj)))

;; TODO: Study `acl2::ld-print-results' more.
(defun acl2-last-return-value ()
  "Returns the last return value sent from ACL2."
  (values-list
   (ensure-list
    acl2::(ld-history-entry-value
           (first (ld-history *the-live-state*))))))


(defun maybe-initialize-acl2 ()
  acl2::(when (not *lp-ever-entered-p*)
          ;; NOTE: in the future we can use this to set the IO to w/e
          ;; stream we'd like by dynamically binding *standard-output*
          ;; to that stream and calling this function.
          (setup-standard-io)
          (set-initial-cbd)
          (setq *lp-ever-entered-p* t)))

(defun eval-in-acl2 (expression)
  "Evaluates EXPRESSION in the ACL2 environment."
  (maybe-initialize-acl2)
  (progn
    acl2::
    ;; NOTE: ACL2 is very eager to intern in every single package! We
    ;; ignore package locks and other warnings using the same
    ;; WITH-SUPPRESSION macro as in ACL2::LP.
    (with-suppression
        (ld-fn
         `((standard-oi . (,slynk-acl2::expression))
           (ld-prompt . nil)
           (ld-verbose . nil)
           (ld-error-triples . t)
           (ld-post-eval-print . nil)
           (ld-error-action . :error))
         *the-live-state*
         nil))
    (acl2-last-return-value)))

(defun read-string-as-acl2 (string &rest reader-options)
  "Reads STRING in as an ACL2 object. READER-OPTIONS are that of
 READ-FROM-STRING."
  (let ((*readtable* acl2::*acl2-readtable*))
    (apply #'cl:read-from-string string reader-options)))


(defun stream-to-string (stream)
  "Turn STREAM into a string object. Destructively modifies STREAM by
repeatably calling READ-LINE on it."
  (reduce (lambda (str rest)
            (format nil "~A~%~A" str rest))
          (loop for line = (read-line stream nil)
                while line
                collect line)
          :initial-value ""))

(defun read-from-string (string &optional (eof-error-p t) eof-value
                                &key (start 0) end preserve-whitespace)
  (let ((cl-interpretation
          (cl:read-from-string
           string eof-error-p eof-value
           :start start :end end
           :preserve-whitespace preserve-whitespace)))
    (if (acl2-expression? cl-interpretation)
        (read-string-as-acl2
         string eof-error-p eof-value
         :start start :end end
         :preserve-whitespace preserve-whitespace)
        cl-interpretation)))

(defun read (&optional (stream *standard-input*)
                       (eof-error-p t)
                       (eof-value nil)
                       (recursive-p nil))
  (when recursive-p
    (warn (format nil "[~S]: RECURSIVE-P is ignored, use CL:READ instead." 'read)))
  (read-from-string
   (stream-to-string stream)
   eof-error-p
   eof-value))

(defun eval (original-exp)
  (if (acl2-expression? original-exp)
      (eval-in-acl2 original-exp)
      (cl:eval original-exp)))


;; Install ourselves into the :slynk and :slynk-mrepl packages.
(shadowing-import
 '(eval read read-from-string)
 :slynk)

(shadowing-import
 '(eval read read-from-string)
 :slynk-mrepl)

;; NOTE: CL inlines inherited symbols at read-time, meaning we cannot
;; just do this simple shadowing of EVAL and READ and have the already
;; loaded :SLYNK and :SLYNK-MREPL packages respect them without
;; reloading.
(load (asdf:system-relative-pathname :slynk "slynk.lisp"))
(load (asdf:system-relative-pathname :slynk/mrepl "../contrib/slynk-mrepl.lisp"))
