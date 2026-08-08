(in-package #:l10n-backend-icu)

(defmethod backend-locale-downcase ((backend icu-backend) string &key locale)
  (declare (ignore backend))
  (call-with-uchars
   string
   (lambda (src src-len)
     (with-foreign-object (err :int)
       (setf (mem-ref err :int) (%zero-error))
       (let ((n (cl-stack-icu:u-str-to-lower (null-pointer) 0 src src-len
                                             (%locale-string locale) err)))
         (setf (mem-ref err :int) (%zero-error))
         (with-foreign-pointer (dest (* (foreign-type-size 'cl-stack-icu:u-char)
                                        (1+ (max n 0))))
           (setf n (cl-stack-icu:u-str-to-lower dest (1+ n) src src-len
                                                (%locale-string locale) err))
           (cl-stack-icu:check-icu (mem-ref err :int) "u-str-to-lower")
           (cl-stack-icu:u-chars-to-lisp dest n)))))))

(defmethod backend-locale-upcase ((backend icu-backend) string &key locale)
  (declare (ignore backend))
  (call-with-uchars
   string
   (lambda (src src-len)
     (with-foreign-object (err :int)
       (setf (mem-ref err :int) (%zero-error))
       (let ((n (cl-stack-icu:u-str-to-upper (null-pointer) 0 src src-len
                                             (%locale-string locale) err)))
         (setf (mem-ref err :int) (%zero-error))
         (with-foreign-pointer (dest (* (foreign-type-size 'cl-stack-icu:u-char)
                                        (1+ (max n 0))))
           (setf n (cl-stack-icu:u-str-to-upper dest (1+ n) src src-len
                                                (%locale-string locale) err))
           (cl-stack-icu:check-icu (mem-ref err :int) "u-str-to-upper")
           (cl-stack-icu:u-chars-to-lisp dest n)))))))

(defmethod backend-locale-titlecase ((backend icu-backend) string &key locale)
  (declare (ignore backend))
  (call-with-uchars
   string
   (lambda (src src-len)
     (with-foreign-object (err :int)
       (setf (mem-ref err :int) (%zero-error))
       (let ((n (cl-stack-icu:u-str-to-title (null-pointer) 0 src src-len
                                             (null-pointer) (%locale-string locale) err)))
         (setf (mem-ref err :int) (%zero-error))
         (with-foreign-pointer (dest (* (foreign-type-size 'cl-stack-icu:u-char)
                                        (1+ (max n 0))))
           (setf n (cl-stack-icu:u-str-to-title dest (1+ n) src src-len
                                                (null-pointer) (%locale-string locale) err))
           (cl-stack-icu:check-icu (mem-ref err :int) "u-str-to-title")
           (cl-stack-icu:u-chars-to-lisp dest n)))))))
