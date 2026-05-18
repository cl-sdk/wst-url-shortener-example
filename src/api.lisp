(in-package #:wst.example.url-shortener)

(defmethod parse-content
    ((type (eql :|application/json|)) content &optional (encoding :utf-8))
  "Defines a request content parser when `request`'s Content-Type is `application/json`.
The parsed object overides the previous request content."
  (declare (ignore type))
  (io.github.cl-sdk.json:parse (content-as-string content encoding)))

(defmethod io.github.cl-sdk.wst.routing.response.dsl:json
    ((implementation (eql :|application/json|)) content response)
  "Defines an implementation for `(json implementation content response)` to transform
the CONTENT to a valid RESPONSE content."
  (log:info 'io.github.cl-sdk.wst.routing.response.dsl:json)
  (io.github.cl-sdk.wst.routing.response.dsl:json t (io.github.cl-sdk.json:stringify content) response))

(defmethod io.github.cl-sdk.wst.request-accept:respond-with
    ((implementation (eql :|application/json|)) content request response)
  "Defines an IMPLEMENTATION to respond when the client says that it accepts `application/json`."
  (log:info 'io.github.cl-sdk.wst.request-accept:respond-with)
  (io.github.cl-sdk.wst.routing.response.dsl:json :|application/json| content response))

(defmethod io.github.cl-sdk.wst.request-accept:respond-with
    ((implementation (eql :|text/csv|)) content request response)
  "Defines an IMPLEMENTATION to respond when the client says that it accepts `text/csv`."
  (headers (list :content-type implementation) response)
  (setf (response-content response)
        (with-output-to-string (s)
          (let ((rows (loop for key being the hash-keys of content
                              using (hash-value value)
                            collect (list key value))))
            (io.github.cl-sdk.csv:write-csv rows s :headers '("code" "url") :always-quote t))))
  response)

(defun response-not-found (request response)
  "Common not found response."
  (with-request-data (accept)
      request
    (serapeum:~>>
     response
     (status 404)
     (io.github.cl-sdk.wst.request-accept:respond-with
      accept
      (cl-hash-util:hash ("message" "not found"))
      request
      _))))

(defun health-handler (request response)
  "Endpoint for health check."
  (declare (ignore request))
  (log:info 'health-handler)
  (serapeum:~>>
   response
   (status 200)
   (text "ok")))

(defun create-short-url-handler (request response)
  "Endpoint to shorten the urls."
  (log:info 'create-short-url-handler)
  (with-request-data (accept app-data)
      request
    (let* ((raw-url (request-url request))
           (target-url (normalize-target-url raw-url)))
      (if (null target-url)
          (serapeum:~>>
           response
           (status 400)
           (io.github.cl-sdk.wst.request-accept:respond-with
            accept
            (cl-hash-util:hash
             ("message" "url field is required and cannot be empty"))
            request
            _))
          (let* ((code (create-short-url app-data target-url))
                 (short-path (format nil "/~a" code))
                 (short-url (format nil "~a~a" (slot-value app-data 'base-url) short-path)))
            (serapeum:~>>
             response
             (status 201)
             (location short-path)
             (io.github.cl-sdk.wst.request-accept:respond-with
              accept
              (cl-hash-util:hash ("code" code)
                                 ("target" target-url)
                                 ("short_url" short-url))
              request _)))))))

(defun list-short-urls-handler (request response)
  "Endpoint to list shorten the urls."
  (log:info 'list-short-urls-handler)
  (with-request-data (accept app-data)
      request
    (serapeum:~>>
     response
     (status 200)
     (io.github.cl-sdk.wst.request-accept:respond-with
      accept
      (all-short-urls app-data)
      request
      _))))

(defun inspect-short-url-handler (request response)
  "Endpoint to retrieve an registered url by code."
  (log:info 'inspect-short-url-handler)
  (with-request-data (accept app-data)
      request
    (let* ((code (request-short-code request))
           (target-url (find-short-url app-data code)))
      (log:info target-url)
      (cond
        ((not (null target-url))
         (serapeum:~>>
          response
          (status 200)
          (io.github.cl-sdk.wst.request-accept:respond-with
           (or accept :|application/json|)
           (cl-hash-util:hash ("code" code)
                              ("target_url" target-url))
           request
           _)))
        (t (response-not-found request response))))))

(defun delete-short-url-handler (request response)
  "Endpoint to delete an url by code."
  (with-request-data (accept app-data)
      request
    (let* ((code (request-short-code request))
           (target-url (find-short-url app-data code)))
      (cond
        ((not (null target-url))
         (progn
           (remove-short-url app-data code)
           (serapeum:~>>
            response
            (status 200)
            (io.github.cl-sdk.wst.request-accept:respond-with
             accept
             (cl-hash-util:hash ("code" code)
                                ("target_url" target-url))
             request
             _))))
        (t (response-not-found request response))))))

(defun redirect-short-url-handler (request response)
  "Endpoint to return a response ready to follow the target url."
  (log:info 'redirect-short-url-handler)
  (with-request-data (app-data)
      request
    (let* ((code (request-short-code request))
           (target-url (find-short-url app-data code)))
      (log:info target-url)
      (cond
        ((not (null target-url))
         (serapeum:~>>
          response
          (status 303)
          (location target-url)
          (text "")))
        (t (response-not-found request response))))))

(defun not-found-handler (request response)
  "Endpoint to accept all invalid paths."
  (response-not-found request response))

(defparameter +parse-content-middleware+
  (parse-request-content)
  "Middlware to parse the content according to the request's `Content-Type`.")

(defparameter +accept-middleware+
  (lambda (request response)
    (with-request-data (route)
        request
      (let* ((request-accept (io.github.cl-sdk.wst.request-accept:parse-request-accept
                              (request-header request "accept" "")))
             (response-accepts (getf (car (io.github.cl-sdk.wst.routing::route-custom route))
                                     :response-accepts))
             (accept (io.github.cl-sdk.wst.request-accept:find-best-response-accept
                      response-accepts
                      request-accept)))
        (log:info request-accept response-accepts accept)
        (append-request-data request :accept (car accept))
        (cons :continue response))))
  "Middlware to transform the resultant object into a response content
 according to the request's `ACCEPT`.")

(defun build-app-routes ()
  "Builds the application routes."
  (condition-handler #'development-condition-handler)

  (io.github.cl-sdk.wst.routing.dsl:build-webserver
   `(:wrap
     :before (,+accept-middleware+
              ,+parse-content-middleware+)
     :route
     (:group
      (:route :GET health "/health" health-handler)
      (:resource
       "/api/v1"
       (:group
        (:resource
         "/links"
         (:route :POST create-short-url create-short-url-handler
             :custom (:response-accepts (:|application/json| :|text/csv| :|text/plain|)))
         (:route :GET list-short-urls list-short-urls-handler
             :custom (:response-accepts (:|application/json| :|text/csv| :|text/plain|)))
         (:route :GET inspect-short-url "/:code" inspect-short-url-handler
           :custom (:response-accepts (:|application/json| :|text/csv| :|text/plain|)))
         (:route :DELETE delete-short-url "/:code" delete-short-url-handler
           :custom (:response-accepts (:|application/json|))))))
      (:route :GET redirect-short-url "/:code" redirect-short-url-handler)
      (:any-route :GET not-found-handler)))))

(defparameter +app-data+
  (make-instance 'app-data)
  "Application state.")

(defun app (env)
  "A woo web server application function to receive all the requests."
  (let* ((request (append-request-data
                   (request-from-woo-env env) :app-data +app-data+))
         (response (dispatch-route request)))
    (response-to-woo-response response)))
