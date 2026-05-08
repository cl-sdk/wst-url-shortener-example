(asdf:defsystem #:wst.example.url-shortener
  :description "Runnable wst URL shortener example application."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:wst.routing
               #:wst.routing.dsl
               #:wst.routing.woo
               #:wst.request-content
               #:wst.request-content.routing
               #:woo)
  :serial t
  :pathname "src"
  :components ((:file "domain")
               (:file "api")
               (:file "main")))
