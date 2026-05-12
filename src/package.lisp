(defpackage #:wst.example.url-shortener
  (:use #:cl
	#:io.github.cl-sdk.wst.routing
	#:io.github.cl-sdk.wst.routing.woo
	#:io.github.cl-sdk.wst.request-content
	#:io.github.cl-sdk.wst.request-content.routing)
  (:import-from #:io.github.cl-sdk.wst.routing.response.dsl
		#:text
		#:headers
		#:json
		#:status))

(in-package #:wst.example.url-shortener)
