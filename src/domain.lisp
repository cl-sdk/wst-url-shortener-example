(in-package #:wst.example.url-shortener)

(defclass app-data ()
  ((next-id :initform 100000000000000000
            :initarg :next-id
            :accessor app-data-next-id
            :documentation "Holds the current id to generate the next url identifier.")
   (base-url :initform "http://localhost:3000"
             :initarg :base-url
             :accessor app-data-base-url
             :documentation "Base CNAME (+ port) to access the short link.")
   (data :initform (make-hash-table :test 'equal)
         :initarg :data
         :accessor app-data-data
         :documentation "Data storage.")
   (lock :initarg :lock
         :initform
         #+sbcl
         (sb-thread:make-mutex :name "url-shortener-store")
         #-sbcl
         nil
         :accessor app-data-lock
         :documentation "Store mutex.")))

(defmacro with-short-url-store-lock (app-data &body body)
  #+sbcl
  `(sb-thread:with-mutex ((app-data-lock ,app-data))
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun generate-next-id (app-data)
  (integer->base62 (incf (app-data-next-id app-data))))

(defun create-short-url (app-data target-url)
  (with-short-url-store-lock app-data
    (let ((candidate (generate-next-id app-data)))
      (setf (gethash candidate (app-data-data app-data)) target-url)
      candidate)))

(defun find-short-url (app-data code)
  (with-short-url-store-lock app-data
    (gethash code (app-data-data app-data))))

(defun remove-short-url (app-data code)
  (with-short-url-store-lock app-data
    (remhash code (app-data-data app-data))))

(defun all-short-urls (app-data)
  (with-short-url-store-lock app-data
    (app-data-data app-data)))

(defun normalize-target-url (url)
  (when (stringp url)
    (let ((trimmed (trim-whitespace url)))
      (unless (zerop (length trimmed))
        (if (starts-with-http-scheme-p trimmed)
            trimmed
            (format nil "https://~a" trimmed))))))
