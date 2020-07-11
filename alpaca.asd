;;;; alpaca.asd

(asdf:defsystem #:alpaca
  :description "Describe alpaca here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:qtools :qtcore :qtgui)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "alpaca")))))


;;; (asdf:load-system :alpaca)
