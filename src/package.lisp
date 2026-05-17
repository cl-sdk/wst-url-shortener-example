(defpackage #:wst.example.url-shortener
  (:use #:cl
	#:io.github.cl-sdk.wst.routing
	#:io.github.cl-sdk.wst.routing.woo
	#:io.github.cl-sdk.wst.request-content
	#:io.github.cl-sdk.wst.request-content.routing)
  (:import-from #:io.github.cl-sdk.wst.routing.response.dsl
		#:location
		#:text
		#:headers
		#:json
		#:status)
  (:documentation "This is a projet playground to test some features 
of the WST's project.

The goal is to provide tools to create web applications in Common Lisp.

You can find the project at:

- https://github.com/cl-sdk/io.github.cl-sdk.wst

LICENSE: Unlicense"))

(in-package #:wst.example.url-shortener)
