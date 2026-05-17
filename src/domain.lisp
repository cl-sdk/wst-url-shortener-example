(in-package #:wst.example.url-shortener)

(defclass url-database ()
  ((next-id :initform 100000000000000000
            :initarg :next-id
            :documentation "Holds the current id to generate the next url identifier.")
   (base-url :initform "http://localhost:3000"
             :initarg :base-url
             :documentation "Base CNAME (+ port) to access the short link.")
   (data :initform (make-hash-table :test 'equal)
         :initarg :data
         :documentation "Data storage.")
   (lock :initarg :lock
         :initform
         #+sbcl
         (sb-thread:make-mutex :name "url-shortener-store")
         #-sbcl
         nil
         :documentation "Store mutex.")))

(defmacro with-short-url-store-lock (url-database &body body)
  #+sbcl
  `(sb-thread:with-mutex ((slot-value ,url-database 'lock))
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun generate-next-id (url-database)
  (integer->base62 (incf (slot-value url-database 'next-id))))

(defun create-short-url (app-data target-url)
  (with-short-url-store-lock app-data
    (let ((candidate (generate-next-id app-data)))
      (setf (gethash candidate (slot-value app-data 'data)) target-url)
      candidate)))

(defun find-short-url (app-data code)
  (with-short-url-store-lock app-data
    (gethash code (slot-value app-data 'data))))

(defun remove-short-url (app-data code)
  (with-short-url-store-lock app-data
    (remhash code (slot-value app-data 'data))))

(defun all-short-urls (app-data)
  (with-short-url-store-lock app-data
    (slot-value app-data 'data)))

(defun normalize-target-url (url)
  (when (stringp url)
    (let ((trimmed (trim-whitespace url)))
      (unless (zerop (length trimmed))
        (if (starts-with-http-scheme-p trimmed)
            trimmed
            (format nil "https://~a" trimmed))))))
