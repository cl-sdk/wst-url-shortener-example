(in-package #:wst.example.url-shortener)

(defparameter *parse-content-middleware*
  (parse-request-content))

(defparameter *base-url* "http://localhost:3000")

(defmethod parse-content
    ((type (eql :|application/json|)) content &optional (encoding :utf-8))
  (declare (ignore type))
  (com.inuoe.jzon:parse (content-as-string content encoding)))

(defun index-handler (request response)
  (declare (ignore request))
  (ok-response t response
	       :content "wst + woo url shortener example\nPOST /api/v1/links with url=... then open GET /:code"))

(defun health-handler (request response)
  (declare (ignore request))
  (ok-response t response :content "ok"))

(defun create-short-url-handler (request response)
  (let* ((raw-url (request-url request))
	 (target-url (normalize-target-url raw-url)))
    (if (null target-url)
	(bad-request-response t response :content "url field is required and cannot be empty")
	(let* ((code (create-short-url target-url))
	       (short-path (format nil "/~a" code))
	       (short-url (format nil "~a~a" *base-url* short-path)))
	  (created-response
	   t
	   response
	   :headers (list :location short-path)
	   :content (format nil "short_url=~a~%code=~a~%target=~a"
			    short-url code target-url))))))

(defun list-short-urls-handler (request response)
  (declare (ignore request))
  (let ((links (sort (all-short-urls) #'string< :key #'car)))
    (ok-response
     t response
     :content (if links
		  (with-output-to-string (out)
		    (dolist (link links)
		      (format out "~a -> ~a~%" (car link) (cdr link))))
		  "no short urls created yet"))))

(defun inspect-short-url-handler (request response)
  (let* ((code (request-short-code request))
	 (target-url (and code (find-short-url code))))
    (if target-url
	(ok-response t response :content (format nil "~a -> ~a" code target-url))
	(not-found-response t response :content "short code not found"))))

(defun delete-short-url-handler (request response)
  (let* ((code (request-short-code request))
	 (target-url (and code (find-short-url code))))
    (if target-url
	(progn
	  (remove-short-url code)
	  (ok-response t response :content (format nil "deleted /~a" code)))
	(not-found-response t response :content "short code not found"))))

(defun redirect-short-url-handler (request response)
  (let* ((code (request-short-code request))
	 (target-url (and code (find-short-url code))))
    (if target-url
	(redirect-see-other-response t response target-url)
	(not-found-response t response :content "short code not found"))))

(defun not-found-handler (request response)
  (declare (ignore request))
  (not-found-response t response :content "route not found"))

(defun build-app-routes ()
  (condition-handler #'development-condition-handler)
  (io.github.cl-sdk.wst.routing.dsl:build-webserver
   `(io.github.cl-sdk.wst.routing.dsl:group
     (io.github.cl-sdk.wst.routing.dsl:route :GET index "/" index-handler)
     (io.github.cl-sdk.wst.routing.dsl:route :GET health "/health" health-handler)
     (io.github.cl-sdk.wst.routing.dsl:resource
      "/api/v1"
      (io.github.cl-sdk.wst.routing.dsl:wrap
       :before (,*parse-content-middleware*)
       :route
       (io.github.cl-sdk.wst.routing.dsl:resource "/links"
						  (io.github.cl-sdk.wst.routing.dsl:route :POST create-short-url create-short-url-handler)
						  (io.github.cl-sdk.wst.routing.dsl:route :GET list-short-urls list-short-urls-handler)
						  (io.github.cl-sdk.wst.routing.dsl:route :GET inspect-short-url inspect-short-url-handler)
						  (io.github.cl-sdk.wst.routing.dsl:route :DELETE delete-short-url delete-short-url-handler))))
     (io.github.cl-sdk.wst.routing.dsl:route :GET redirect-short-url "/:code" redirect-short-url-handler)
     (io.github.cl-sdk.wst.routing.dsl:any-route :GET not-found-handler))))

(defun app (env)
  (let* ((request (request-from-woo-env env))
	 (response (dispatch-route request)))
    (response-to-woo-response response)))
