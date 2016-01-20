;;;; newhope.asd

(asdf:defsystem #:newhope
  :description "Describe newhope here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:plump
               #:clss
               #:cl-json
               #:cl-cron
               #:postmodern
               #:burgled-batteries)
  :serial t
  :components ((:file "package")
               (:file "newhope")
               (:file "map")
               (:file "51job_spider")
               (:file "zhaopin_spider")
               (:file "lagou_spider")
               (:file "neitui_spider")))
