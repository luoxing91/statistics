(defpackage :tk.luoxing123.web
  (:use :cl :net.aserve ));:com.luoxing123.html
(in-package :tk.luoxing123.web)
(start :port 1991)
(publish-file :path "/hello.html" :file "/tmp/html/hello.html")
(publish-file :path "/hello.html" :remove t)
(publish-directory :prefix "/" :destination "/tmp/html")
(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
	(with-http-body (request entity)
	  (format (request-reply-stream request)
			  "<html>~@
<head><title>Random</title></head>~@
<body>~@
<p>Random number: ~d</p>~@
</body>~@
</html>~@"
			  (random 1000)))))
(publish-file :path "/random-number" :function 'random-number)
(emit-html '(:html (:head (:title "Hello")) (:body (:p "hello,word"))))
(define-html-macro :standard-page ((&key title) &body body )
  '(:html (:head (:title ,title))
	(:body (:h1 ,title)
	 ,@body)))
then you use tag :standard-page in your-expression html, 
(defun show-query-params (request entity)
  (with-http-response (request entity :content-type "text/html")
	(with-http-body (request entity)
	  (with-html-body ((request-reply-stream request))
		(html (:standard-page
			   (:title "Query Parameters")
			   (if (request-query request)
				   (html
					 (:table :border 1
							 (loop for (k .v) in (request-query request)
								  do (html (:tr (:td k) (:td v))))))
				   (html (:p "No query parameters")))))))))

(defun simple-form (request entity)
  (with-http-response (request entity :content-type "text/html")
	(with-http-body (request entity)
	  (let ((*html-output* (request-reply-stream request)))
		(html (:html
				(:head (:title "simple form"))
				(:body
				 (:form :method "POST" :action "/show-query-params"
						(:table
						 (:tr (:td "Foo")
							  (:td (:input :name "foo" :size 20)))
						 (:tr (:td "password")
							  (:td (:input :name "password"
										   :type "password"
										   :size 20)))
						 )(:p (:input :name "submit"
									  :type "submit"
									  :value "okay")
							  (:input ::type "resset" :value "reset"))))))))))
(publish :path "/simple-form" :function 'simple-form)
(defun show-cookies (request entity)
  (with-http-response (request entity :content-type "text/html")
	(with-http-body (request entity)
	  (with-html-output ((request-reply-stream request))
		(html
		  (:standard-page
		   (:title "cookies")
		   (if (null (get-cookie-values request))
			   (html
				 (:table
				  (loop for (key . value) in (get-cookie-values request)
					 do (html (:tr (:td key ) (:td value)))))))))))))
(defun set-cookie (request entity)
  (with-http-response (request entity :content-type "text/html")
	(set-cookie-header request :name "myCookie" :value "A cookie value")
	(with-http-body (request entity)
	  (html
		(:standard-page
		 (:title "set cookie")
		 (:p "cookies set")
		 (:p (:a :href "/show-cookies" "look at cookie jar")))))))
(publish :path "/set-cookie" :function 'set-cookie)
(define-url-function random-number (request (limit integer 1000))
  (:html
	(:head (:title "random"))
	(:body
	 (:p "random number" (:print (random limit))))))
(defmacro define-url-function (name (request &rest params) &body body)
  (with-gensyms (entity)
	(let ((params (mapcar #'normalize-param params)))
	  `(progn
		 (defun ,name (,request ,entity)
		   (with-http-response (,request ,entity :content-type "text/html")
			 (let* (,@(param-bindings name request params))
			   ,@(set-cookies-code name request params)
			   (with-http-body (,request ,entity)
				 (with-html-body ((request-reply-stream ,request))
				   (html ,@body))))))
		 (publish :path ,(format nil "/~(~a~)" name) :function ',name)))))
