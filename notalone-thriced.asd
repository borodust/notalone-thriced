(asdf:defsystem :notalone-thriced
  :description "Autumn 2021 Lisp Game Jam Entry"
  :version "0.0.0"
  :license "GPLv3"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (:notalone-thriced/game :notalone-thriced/tools))


(asdf:defsystem :notalone-thriced/game
  :description "End-user portion of NOTALONE-THRICED"
  :version "0.0.0"
  :license "GPLv3"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (#:alien-works
               #:dissect
               #:float-features)
  :serial t
  :pathname "game/"
  :components ((:file "packages")
               (:file "utils")
               (:file "game")))


(asdf:defsystem :notalone-thriced/tools
  :description "Tools for NOTALONE-THRICED"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (#:notalone-thriced/game
               #:alien-works/tools)
  :serial t
  :pathname "tools/"
  :components ((:file "packages")
               (:file "eden")))


(asdf:defsystem :notalone-thriced/bundle
  :description "Bundle for NOTALONE-THRICED"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (#:notalone-thriced/game #:alien-works-delivery)
  :serial t
  :pathname "bundle/"
  :components ((:file "bundle")))