(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:wst.example.url-shortener)
    (defpackage #:wst.example.url-shortener
      (:use #:cl))))

(in-package #:wst.example.url-shortener)

(defparameter *short-url-store* (make-hash-table :test 'equal))
(defparameter *next-short-id* 0)
(defparameter +base62-alphabet+ "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defconstant +http-scheme-prefix+ "http://")
(defconstant +https-scheme-prefix+ "https://")
#+sbcl
(defparameter *short-url-store-lock*
  (sb-thread:make-mutex :name "woo-url-shortener-store"))

(defmacro with-short-url-store-lock (&body body)
  #+sbcl
  `(sb-thread:with-mutex (*short-url-store-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

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

(defun trim-whitespace (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return) text))

(defun starts-with-http-scheme-p (url)
  (or (and (>= (length url) (length +http-scheme-prefix+))
           (string-equal +http-scheme-prefix+ url :end2 (length +http-scheme-prefix+)))
      (and (>= (length url) (length +https-scheme-prefix+))
           (string-equal +https-scheme-prefix+ url :end2 (length +https-scheme-prefix+)))))

(defun normalize-target-url (url)
  (when (stringp url)
    (let ((trimmed (trim-whitespace url)))
      (unless (zerop (length trimmed))
        (if (starts-with-http-scheme-p trimmed)
            trimmed
            (format nil "https://~a" trimmed))))))
