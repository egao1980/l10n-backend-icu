(in-package #:l10n-backend-icu)

(defmethod backend-make-collator ((backend icu-backend) &key locale strength options)
  (declare (ignore backend options))
  (with-foreign-object (err :int)
    (setf (mem-ref err :int) (%zero-error))
    (let ((raw (cl-stack-icu:ucol-open (%locale-string locale) err)))
      (cl-stack-icu:check-icu (mem-ref err :int) "ucol-open")
      (when strength
        (cl-stack-icu:ucol-set-strength raw (%strength-enum (or strength :tertiary))))
      (make-instance 'collator
                     :locale locale
                     :strength (or strength :tertiary)
                     :raw raw))))

(defmethod backend-collate ((backend icu-backend) collator string-a string-b)
  (declare (ignore backend))
  (with-foreign-object (err :int)
    (setf (mem-ref err :int) (%zero-error))
    (with-foreign-strings ((a string-a :encoding :utf-8)
                           (b string-b :encoding :utf-8))
      (let ((r (cl-stack-icu:ucol-strcoll-utf8 (collator-raw collator)
                                               a -1 b -1 err)))
        (cl-stack-icu:check-icu (mem-ref err :int) "ucol-strcoll-utf8")
        r))))

(defmethod backend-sort-key ((backend icu-backend) collator string)
  (declare (ignore backend))
  (call-with-uchars
   string
   (lambda (src src-len)
     (let ((cap (max 64 (* src-len 4))))
       (with-foreign-pointer (buf cap)
         (let ((n (cl-stack-icu:ucol-get-sort-key (collator-raw collator)
                                                  src src-len buf cap)))
           (when (> n cap)
             (with-foreign-pointer (buf2 n)
               (setf n (cl-stack-icu:ucol-get-sort-key (collator-raw collator)
                                                       src src-len buf2 n))
               (return-from backend-sort-key
                 (let ((out (make-array (max 0 (1- n)) :element-type '(unsigned-byte 8))))
                   (dotimes (i (length out) out)
                     (setf (aref out i) (mem-aref buf2 :uint8 i)))))))
           (let ((out (make-array (max 0 (1- n)) :element-type '(unsigned-byte 8))))
             (dotimes (i (length out) out)
               (setf (aref out i) (mem-aref buf :uint8 i))))))))))
