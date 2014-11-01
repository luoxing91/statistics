(defpackage :tk.luoxing123.id3
  (:use :common-lisp
        :tk.luoxing123.pathnames)
  (:export
   :read-id3))
(define-binary-type unsigned-integer (bytes)
  (:reader (in)
           (loop with value =0
                for low-bit downfrom () to 0 by 8 do
                (setf (ldb (byte 8 low-bit) value ) (read-byte in))
                finally (return value)))
  (:writer (out value)
           (loop for low-bit downfrom (* 8 (1- bytes)) to 0 by 8
                do (write-byte (ldb (byte 8 low-bit) out)))))
