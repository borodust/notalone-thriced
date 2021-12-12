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
               #:float-features
               #:cffi
               #:cffi-c-ref
               #:local-time)
  :serial t
  :pathname "game/"
  :components ((:file "packages")
               (:file "utils")
               (:module "framework"
                :components ((:file "tools")
                             (:file "state")
                             (:file "resource")
                             (:file "cloud")
                             (:file "banner")
                             (:file "floor")
                             (:file "main")))
               (:module "implementation"
                :components ((:file "state")
                             (:file "record")
                             (:file "game")
                             (:file "play")
                             (:file "end")))))


(asdf:defsystem :notalone-thriced/tools
  :description "Tools for NOTALONE-THRICED"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (#:notalone-thriced/game
               #:alien-works/tools

               #:slynk
               #:slynk/mrepl
               #:slynk/arglists
               #:slynk/fancy-inspector
               #:slynk/package-fu
               #:slynk/trace-dialog
               #:slynk/stickers
               #:slynk/indentation)
  :serial t
  :pathname "tools/"
  :components ((:file "packages")
               (:module "assets"
                :components ((:file "converter")
                             (:file "resources")))
               (:module "eden"
                :components ((:file "eden")
                             (:file "ui")))))


(asdf:defsystem :notalone-thriced/bundle
  :description "Bundle for NOTALONE-THRICED"
  :version "0.0.0"
  :license "MIT"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :depends-on (#:alien-works-delivery)
  :serial t
  :pathname "bundle/"
  :components ((:file "bundle")))
