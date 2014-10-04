;;;; orc-battle.asd

(asdf:defsystem #:orc-battle
  :serial t
  :description "An epic battle against orcs. And other stuff."
  :author "Sergi Reyner <sergi.reyner@gmail.com>"
  :license "MIT"
  :depends-on (#:ansi-color #:lispbuilder-sdl)
  :components ((:file "package")
               (:file "player")
               (:file "monster")
               (:file "orc")
               (:file "hydra")
               (:file "slime-mold")
               (:file "brigand")
               (:file "orc-battle")))
