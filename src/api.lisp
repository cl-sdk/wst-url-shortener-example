(in-package #:wst.example.url-shortener)

(defparameter *parse-content-middleware*
  (parse-request-content))

(defparameter *base-url* "http://localhost:3000")

(defmethod parse-content
    ((type (eql :|application/json|)) content &optional (encoding :utf-8))
  (declare (ignore type))
  (io.github.cl-sdk.json:parse (content-as-string content encoding)))

(defmethod io.github.cl-sdk.wst.routing.response.dsl:json ((implementation (eql ':json)) content response)
  (call-next-method t (io.github.cl-sdk.json:stringify content) response))

(defun health-handler (request response)
  (declare (ignore request))
  (serapeum:~>>
   response
   (status 200)
   (json :json
	 (cl-hash-util:hash ("status" "ok")))))

(defun create-short-url-handler (request response)
  (let* ((raw-url (request-url request))
	 (target-url (normalize-target-url raw-url)))
    (if (null target-url)
	(serapeum:~>>
	 response
	 (status 400)
	 (json :json
	       (cl-hash-util:hash
		("message" "url field is required and cannot be empty"))))
	(let* ((code (create-short-url target-url))
	       (short-path (format nil "/~a" code))
	       (short-url (format nil "~a~a" *base-url* short-path)))
	  (serapeum:~>>
	   response
	   (status 201)
	   (location short-path)
	   (json :json (cl-hash-util:hash ("code" code)
					  ("target" target-url)
					  ("short_url" short-url))))))))

(defun list-short-urls-handler (request response)
  (declare (ignore request))
  (let ((links (all-short-urls)))
    (serapeum:~>>
     response
     (status 200)
     (json :json links))))

(defun response-not-found (response)
  (serapeum:~>>
   response
   (status 404)
   (json :json (cl-hash-util:hash ("message" "not found")))))

(defun inspect-short-url-handler (request response)
  (let* ((code (request-short-code request))
	 (target-url (find-short-url code)))
    (if target-url
	(serapeum:~>>
	 response
	 (status 200)
	 (json :json (cl-hash-util:hash ("code" code)
					("target_url" target-url))))
	(response-not-found response))))

(defun delete-short-url-handler (request response)
  (let* ((code (request-short-code request))
	 (target-url (find-short-url code)))
    (if target-url
	(progn
	  (remove-short-url code)
	  (serapeum:~>>
	   response
	   (status 200)
	   (json :json (cl-hash-util:hash ("code" code)
					  ("target_url" target-url)))))
	(response-not-found response))))

(defun redirect-short-url-handler (request response)
  (let* ((code (request-short-code request))
	 (target-url (find-short-url code)))
    (if target-url
	(serapeum:~>>
	 response
	 (status 303)
	 (location target-url)
	 (text ""))
	(response-not-found response))))

(defun not-found-handler (request response)
  (declare (ignore request))
  (response-not-found response))

(defun build-app-routes ()
  (condition-handler #'development-condition-handler)

  (io.github.cl-sdk.wst.routing.dsl:build-webserver
   `(:wrap
     :before (,*parse-content-middleware*)
     :route
     (:group
      (:route :GET health "/health" health-handler)
      (:resource
       "/api/v1"
       (:group
	(:resource
	 "/links"
	 (:route :POST create-short-url create-short-url-handler)
	 (:route :GET list-short-urls list-short-urls-handler)
	 (:route :GET inspect-short-url "/:code" inspect-short-url-handler)
	 (:route :DELETE delete-short-url "/:code" delete-short-url-handler))))
      (:route :GET redirect-short-url "/:code" redirect-short-url-handler)
      (:any-route :GET not-found-handler)))))

(defun app (env)
  (let* ((request (request-from-woo-env env))
	 (response (dispatch-route request)))
    (response-to-woo-response response)))
