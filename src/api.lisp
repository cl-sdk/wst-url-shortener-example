(in-package #:wst.example.url-shortener)

(defparameter *parse-content-middleware*
  (wst.request-content.routing:parse-request-content))

(defparameter *base-url* "http://localhost:3000")

(defun request-url (request)
  (let ((body (wst.routing:request-content request)))
    (cond
      ((hash-table-p body)
       (or (gethash "url" body)
           (gethash :url body)))
      ((listp body)
       (or (cdr (assoc "url" body :test #'string=))
           (cdr (assoc :url body))))
      (t nil))))

(defun index-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response
   t response
   :content
   "wst + woo url shortener example\nPOST /api/v1/links with url=... then open GET /:code"))

(defun health-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response t response :content "ok"))

(defmethod wst.request-content:parse-content
    ((type (eql :|application/json|)) content &optional (encoding :utf-8))
  (declare (ignore type))
  (com.inuoe.jzon:parse (wst.request-content:content-as-string content encoding)))

(defun create-short-url-handler (request response)
  (let* ((raw-url (request-url request))
         (target-url (normalize-target-url raw-url)))
    (if (null target-url)
        (wst.routing:write-response response :status 400 :content "url field is required and cannot be empty")
        (let* ((code (create-short-url target-url))
               (short-path (format nil "/~a" code))
               (short-url (format nil "~a~a" *base-url* short-path)))
          (wst.routing:write-response
           response
           :status 201
           :headers (list :location short-path)
           :content (format nil "short_url=~a~%code=~a~%target=~a"
                            short-url code target-url))))))

(defun request-short-code (request)
  (wst.routing:with-request-data (params) request
    (cdr (assoc "code" params :test #'string-equal))))

(defun list-short-urls-handler (request response)
  (declare (ignore request))
  (let ((links (sort (all-short-urls) #'string< :key #'car)))
    (wst.routing:ok-response
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
        (wst.routing:ok-response t response :content (format nil "~a -> ~a" code target-url))
        (wst.routing:not-found-response t response :content "short code not found"))))

(defun delete-short-url-handler (request response)
  (let* ((code (request-short-code request))
         (target-url (and code (find-short-url code))))
    (if target-url
        (progn
          (remove-short-url code)
          (wst.routing:ok-response t response :content (format nil "deleted /~a" code)))
        (wst.routing:not-found-response t response :content "short code not found"))))

(defun redirect-short-url-handler (request response)
  (let* ((code (request-short-code request))
         (target-url (and code (find-short-url code))))
    (if target-url
        (wst.routing:redirect-see-other-response t response target-url)
        (wst.routing:not-found-response t response :content "short code not found"))))

(defun not-found-handler (request response)
  (declare (ignore request))
  (wst.routing:not-found-response t response :content "route not found"))

(defun build-app-routes ()
  (wst.routing:condition-handler #'wst.routing:development-condition-handler)
  (wst.routing.dsl:build-webserver
   `(wst.routing.dsl:group
     (wst.routing.dsl:route :GET index "/" index-handler)
     (wst.routing.dsl:route :GET health "/health" health-handler)
     (wst.routing.dsl:resource "/api/v1"
                               (wst.routing.dsl:wrap
                                :before (,*parse-content-middleware*)
                                :route (wst.routing.dsl:route :POST create-short-url "/links" create-short-url-handler))
                               (wst.routing.dsl:route :GET list-short-urls "/links" list-short-urls-handler)
                               (wst.routing.dsl:route :GET inspect-short-url "/links/:code" inspect-short-url-handler)
                               (wst.routing.dsl:route :DELETE delete-short-url "/links/:code" delete-short-url-handler))
     (wst.routing.dsl:route :GET redirect-short-url "/:code" redirect-short-url-handler)
     (wst.routing.dsl:any-route :GET not-found-handler))))
