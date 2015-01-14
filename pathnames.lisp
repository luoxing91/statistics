(defpackage :tk.luoxing123.pathnames
  (:use :common-lisp :tk.luoxing123.utility)
  (:export file-exists-p))
(defun file-exists-p(path)
  (probe-file pathname) )
  
;;implicit
(defun directory-pathname-p(p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory(name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "can't reliably convert wild pathnames."))
  (if (not (directory-pathname-p name))
      (make-pathname
       :directory (append (list (file-namestring pathname)))
       :name nil
       :type nil
       :defaults pathname)
      pathname))
(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type :wild
   :defaults (pathname-as-directory dirname)))

;;;
(defun list-directory(dirname)
  (when (wild-pathname-p dirname)
    (error "can only list concrete directory names."))
  (directory (directory-wildcard dirname)))

    
(defun component-present-p(value)
  (and value (not (eql value :unspecific))))
