(in-package :common-lisp-user)
(defun hello()
  (write-string "hello, world!"))
;allow a running cl process to accept a Telent login on port 4000 on
										;the
(defun start-telnet (&optional (port 400))
  (let ((passive (socket:make-socket :connect :passive
									 :local-host "127.l"
									 :local-port port
									 :reuse-address t)))
	(mp:process-run-function
	 "telnet-listener"
	 #'(lambda (pass)
		 (let ((count 0))
		   (loop
				(let ((con (socket:accept-connection pass)))
				  (mp:process-run-function
				   (format nil "tel~d" (incf count))
				   #'(lambda (con)
					   (unwind-protect
							(tpl::start-interactive-top-level
							 con
							 #'tpl::top-level-read-eval-print-loop
							 nil)
						 (ignore-errors (close con :abort t)))))
				  con))))
	 passive)))
