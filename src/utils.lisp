(in-package #:wst.example.url-shortener)

(defconstant +base62-alphabet+ "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defconstant +http-scheme-prefix+ "http://")
(defconstant +https-scheme-prefix+ "https://")

(defun integer->base62 (value)
  "Convert an integer VALUE to a base 62 string."
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
  "Trim a TEXT string."
  (string-trim '(#\Space #\Tab #\Newline #\Return) text))

(defun starts-with-http-scheme-p (url)
  "Check if URL starts with a HTTP(s) scheme prefix."
  (or (and (>= (length url) (length +http-scheme-prefix+))
        (string-equal +http-scheme-prefix+ url :end2 (length +http-scheme-prefix+)))
     (and (>= (length url) (length +https-scheme-prefix+))
        (string-equal +https-scheme-prefix+ url :end2 (length +https-scheme-prefix+)))))

(defun request-url (request)
  "Get the url from the REQUEST's content."
  (let ((body (request-content request)))
    (cond
      ;; NOTE: if Content-Type is JSON.
      ((hash-table-p body)
       (or (gethash "url" body)
          (gethash :url body)))
      ;; NOTE: if Content-Type is multipart/form-data
      ((listp body)
       (or (cdr (assoc "url" body :test #'string=))
          (cdr (assoc :url body))))
      (t nil))))

(defun request-short-code (request)
  "Returns the REQUEST's code parameter."
  (with-request-data (params) request
    (cdr (assoc "code" params :test #'string-equal))))
