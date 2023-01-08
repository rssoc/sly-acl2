;;; sly-acl2.el -- ACL2 support for sly.
;;;
(require 'sly)


(define-sly-contrib sly-acl2
  "ACL2 support for sly."
  (:slynk-dependencies slynk-acl2))

;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-acl2 'append))
(provide 'sly-acl2)
