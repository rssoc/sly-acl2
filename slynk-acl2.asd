(in-package :asdf-user)

(defsystem :slynk-acl2
  :author "rssoc"
  :description "ACL2 support for Slynk."
  :depends-on (:slynk :acl2)
  :components ((:file "slynk-acl2")))
