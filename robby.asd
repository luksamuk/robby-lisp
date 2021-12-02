(asdf:defsystem #:robby
  :description "Robby, the Soda-Can-Collecting Robot"
  :author "Lucas S. Vieira <lucasvieira@protonmail.com>"
  :version "0.1.0"
  :license "MIT"
  :serial t
  :depends-on (#:lparallel
               #:cl-cpus)
  :components ((:file "package")
               (:module "src"
                :components ((:file "params")
                             (:file "specimen")
                             (:file "board")
                             (:file "game-state")
                             (:file "fitness")
                             (:file "population")
                             (:file "main")))))

