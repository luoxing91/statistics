(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;;;;;;;;;;;;;;;;;;;;;;;
(defvar *db* nil)
;;writer
(defun make-cd (title artist rating ripped)
  `(:title ,title :artist ,artist :rating ,rating :ripped ,ripped))
(defun add-record (cd)
  (push cd  *db*))
;;selecter

(defmacro where-1 (&rest clauses)
  `#'(lambda (cd)
	   (and
		,@(loop while clauses
			 collecting `(equal (getf cd ,(pop clauses) ) ,(pop clauses))))))

(defun select-by-artist (artist)
  (select (artist-sector artist)))
;;update
(defun update (selector &key title artist rating (ripped nil ripped-p))
  (setf *db
		(mapcar #'(lambda (row)
					(when (funcall selector row)
					  (if title (setf (getf row :title) title))
					  (if artist (setf (getf row :artist) artist))
					  (if rating (setf (getf row :rating) rating))
					  (if ripped-p (setf (getf row :ripped) ripped)))
					row)
				*db*)))
(defun delete-row (selector)
  (setf *db* (remove-if selector *db*)))
;;persistence
(defun save-db (filename)
  (with-open-file (out filename
					   :direction :output
					   :if-exists :supersede)
	(with-standard-io-syntax (print *db* out))))
(defun load-db (filename)
  (with-open-file (in filename)
	(with-standard-io-syntax (setf *db* (read in)))))
;;print 
(defun dump-db (&optional (db *db*) )
  (format t "~{~{~a: ~10t ~a~%~}~%~}" db))
;;interactive
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Rating")
   (prompt-read "ripped [y/n")))
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
	 (if (not (y-or-n-p "Another? y/n:")) (return))))
;;;;;;;;;;;;;;;;;;;;;;never mind the below ;;;;;;;;;;;;
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))
(defun artist-sector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))
(defun title-sector (title)
  #'(lambda (cd) (equal (getf cd :title) title)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))
(defun make-comparison-lst (fields)
  (loop while fields
	   collecting (make-comparison-expr (pop fields) (pop fields))))
(defun where-2 (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
	  (and
	   (if title (equal (getf cd :title) title) t)
	   (if artist (equal (getf cd :artist) artist) t)
	   (if rating (equal (getf cd :rating) rating) t)
	   (if ripped-p (equal (getf cd :ripped) ripped)t))))


(add-record (make-cd "rose" "kathy Mattes" 7 t))
(add-record (make-cd "fly" "Dixie chicks" 8 t))
(add-record (make-cd "home" "dixie chicke" 9 t))

