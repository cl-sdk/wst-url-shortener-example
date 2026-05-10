(in-package #:wst.example.url-shortener)

(defparameter *short-url-store* (make-hash-table :test 'equal))
(defparameter *next-short-id* 0)
#+sbcl
(defparameter *short-url-store-lock*
  (sb-thread:make-mutex :name "woo-url-shortener-store"))

(defmacro with-short-url-store-lock (&body body)
  #+sbcl
  `(sb-thread:with-mutex (*short-url-store-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun create-short-url (target-url)
  (with-short-url-store-lock
    (let ((candidate (integer->base62 (incf *next-short-id*))))
      (setf (gethash candidate *short-url-store*) target-url)
      candidate)))

(defun find-short-url (code)
  (with-short-url-store-lock
    (gethash code *short-url-store*)))

(defun remove-short-url (code)
  (with-short-url-store-lock
    (remhash code *short-url-store*)))

(defun all-short-urls ()
  (with-short-url-store-lock
    (loop for code being the hash-keys of *short-url-store*
          using (hash-value target-url)
          collect (cons code target-url))))

(defun normalize-target-url (url)
  (when (stringp url)
    (let ((trimmed (trim-whitespace url)))
      (unless (zerop (length trimmed))
        (if (starts-with-http-scheme-p trimmed)
            trimmed
            (format nil "https://~a" trimmed))))))
