;;;; alpaca.asd

(asdf:defsystem #:alpaca
  :description "Describe alpaca here"
  :author "mikel evins <mevins@me.com>"
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "alpaca")))))

;;; (asdf:load-system :alpaca)
