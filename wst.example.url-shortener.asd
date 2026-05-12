(asdf:defsystem #:wst.example.url-shortener
  :description "Runnable wst URL shortener example application."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:io.github.cl-sdk.wst.routing
               #:io.github.cl-sdk.wst.routing.dsl
               #:io.github.cl-sdk.wst.routing.woo
               #:io.github.cl-sdk.wst.request-content
               #:io.github.cl-sdk.wst.request-content.routing
	       #:io.github.cl-sdk.json
               #:woo)
  :serial t
  :pathname "src"
  :components ((:file "package")
	       (:file "utils")
               (:file "domain")
               (:file "api")
               (:file "main")))
