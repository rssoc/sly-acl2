(in-package :asdf-user)

(defsystem :slynk-acl2
  :author "rssoc"
  :description "ACL2 support for Slynk."
  :depends-on (:slynk :slynk/mrepl)
  :components ((:file "slynk-acl2"
                :if-feature :acl2)
               (:file "slynk-mrepl-acl2"
                :if-feature :acl2)))

(defmethod perform :after (o (c (eql (find-system :acl2))))
  (clear-system :slynk-acl2)
  (load-system :slynk-acl2))
