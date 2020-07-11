;;;; alpaca.asd

(asdf:defsystem #:alpaca
  :description "Alpaca: a programmable editor for authors"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version "0.9.1"
  :homepage "https://github.com/mikelevins/alpaca"
  :serial t
  :depends-on (:qtools :qtcore :qtgui :cl-ppcre :trivial-gray-streams)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "alpaca")))))


;;; (asdf:load-system :alpaca)
;;; (alpaca:main)
