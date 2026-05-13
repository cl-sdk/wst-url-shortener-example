(in-package #:wst.example.url-shortener)

(defconstant +base62-alphabet+ "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defconstant +http-scheme-prefix+ "http://")
(defconstant +https-scheme-prefix+ "https://")

(defun integer->base62 (value)
  (if (zerop value)
      "0"
      (loop with result = ""
	    with quotient = value
	    while (> quotient 0)
	    for remainder = (mod quotient 62)
	    do (setf result (concatenate 'string
					 (string (char +base62-alphabet+ remainder))
					 result)
		     quotient (floor quotient 62))
	    finally (return result))))

(defun trim-whitespace (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return) text))

(defun starts-with-http-scheme-p (url)
  (or (and (>= (length url) (length +http-scheme-prefix+))
	(string-equal +http-scheme-prefix+ url :end2 (length +http-scheme-prefix+)))
     (and (>= (length url) (length +https-scheme-prefix+))
	(string-equal +https-scheme-prefix+ url :end2 (length +https-scheme-prefix+)))))

(defun request-url (request)
  (let ((body (request-content request)))
    (cond
      ((hash-table-p body)
       (or (gethash "url" body)
	  (gethash :url body)))
      ((listp body)
       (or (cdr (assoc "url" body :test #'string=))
	  (cdr (assoc :url body))))
      (t nil))))

(defun request-short-code (request)
  (with-request-data (params) request
    (cdr (assoc "code" params :test #'string-equal))))
