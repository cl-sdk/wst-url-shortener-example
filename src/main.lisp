(in-package #:wst.example.url-shortener)

(defparameter *server-port* 3000)
(defparameter *server-running-p* nil)
(defparameter *restart-requested-p* nil)
#+sbcl
(defparameter *server-control-lock*
  (sb-thread:make-mutex :name "woo-example-server-control"))

(defconstant +sigint+ 2)
(defconstant +sigquit+ 3)
(defconstant +sigterm+ 15)

(defun woo-signal-symbol (name)
  (or (find-symbol name :woo.signal)
     (error "Woo internal symbol ~a not found in package WOO.SIGNAL" name)))

(defun make-graceful-shutdown-signals ()
  "Map SIGINT/SIGQUIT/SIGTERM to Woo's graceful shutdown callback."
  (let ((graceful-callback-symbol (woo-signal-symbol "SIGQUIT-CB")))
    (list (cons +sigint+ graceful-callback-symbol)
	  (cons +sigquit+ graceful-callback-symbol)
	  (cons +sigterm+ graceful-callback-symbol))))

(defmacro with-server-control-lock (&body body)
  #+sbcl
  `(sb-thread:with-mutex (*server-control-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun request-graceful-stop ()
  #+sbcl
  (sb-posix:kill (sb-posix:getpid) +sigquit+)
  #-sbcl
  (error "Restarting a running server is only supported on SBCL."))

(defun start-server (&key (port 3000))
  (with-server-control-lock
    (when *server-running-p*
      (error "The example app is already running. Use (restart-server) to restart it."))
    (setf *server-port* port
	  *restart-requested-p* nil))
  (loop
    (build-app-routes)
    (format t "~&Starting example app on http://localhost:~a~%" *server-port*)
    (format t "~&Press Ctrl+C to stop gracefully.~%")
    (let ((signals-symbol (woo-signal-symbol "*SIGNALS*")))
      (unwind-protect
	   (progn
	     (with-server-control-lock
	       (setf *server-running-p* t))
	     (progv (list signals-symbol) (list (make-graceful-shutdown-signals))
	       (woo:run #'app :port *server-port*)))
	(with-server-control-lock
	  (setf *server-running-p* nil))))
    (unless (with-server-control-lock
	      (prog1 *restart-requested-p*
		(setf *restart-requested-p* nil)))
      (return))
    (format t "~&Restarting example app...~%")))

(defun restart-server (&key (port *server-port*))
  (multiple-value-bind (running-p target-port)
      (with-server-control-lock
	(setf *server-port* port)
	(if *server-running-p*
	    (progn
	      (setf *restart-requested-p* t)
	      (values t *server-port*))
	    (values nil *server-port*)))
    (if running-p
	(request-graceful-stop)
	(start-server :port target-port))))

(start-server)
