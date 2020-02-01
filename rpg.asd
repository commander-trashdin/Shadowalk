;;;; rpg.asd

(asdf:defsystem #:rpg
  :description "Shadowalk"
  :author "thrashdin aun.sokolov@gmail.com>"
  :license  "CCA"
  :version "0.0.2"
  :serial t
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "world-state")
                 (:file "utils")
                 (:file "entities")
                 (:file "interface")))
               (:module "log")))
