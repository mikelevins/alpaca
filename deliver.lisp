(in-package "CL-USER")

(load-all-patches)

(require :asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:defsystem #:alpaca
  :description "Describe alpaca here"
  :author "mikel evins <mevins@me.com>"
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "alpaca")))))
(defun load-alpaca ()
  (asdf:oos 'asdf:compile-op :alpaca)
  (asdf:oos 'asdf:load-op :alpaca))

(load-alpaca)

(defparameter *target-application-path* "/usr/local/src/alpaca/Alpaca.app")

(deliver 'alpaca-main
         (create-macos-application-bundle
          *target-application-path*
          :template-bundle "/usr/local/src/alpaca/assets/bundle/Alpaca.app/")
         0
         :interface :capi
         :quit-when-no-windows nil :keep-eval t :keep-macros t
         :keep-lisp-reader t :keep-load-function :full
         :print-circle t :keep-editor t :keep-clos t
         :keep-clos-object-printing t :editor-style :emacs)
