(in-package #:l10n-backend-icu)

(defun %number-style (style)
  (foreign-enum-value 'cl-stack-icu:u-number-format-style
                      (ecase (or style :decimal)
                        (:decimal :decimal)
                        (:percent :percent)
                        (:scientific :scientific)
                        (:currency :currency)
                        (:currency-iso :currency-iso)
                        (:currency-plural :currency-plural)
                        (:currency-accounting :currency-accounting)
                        (:cash-currency :cash-currency)
                        (:currency-standard :currency-standard)
                        (:default :default))))

(defun %date-style (style)
  (foreign-enum-value 'cl-stack-icu:u-date-format-style
                      (ecase (or style :short)
                        (:full :full)
                        (:long :long)
                        (:medium :medium)
                        (:short :short)
                        (:default :default)
                        (:none :none))))

(defun %format-double (style value locale)
  (with-foreign-object (err :int)
    (setf (mem-ref err :int) (%zero-error))
    (let ((fmt (cl-stack-icu:unum-open (%number-style style)
                                       (null-pointer) 0
                                       (%locale-string locale)
                                       (null-pointer) err)))
      (cl-stack-icu:check-icu (mem-ref err :int) "unum-open")
      (unwind-protect
           (progn
             (setf (mem-ref err :int) (%zero-error))
             (let ((n (cl-stack-icu:unum-format-double fmt (float value 1d0)
                                                       (null-pointer) 0
                                                       (null-pointer) err)))
               (setf (mem-ref err :int) (%zero-error))
               (with-foreign-pointer (buf (* (foreign-type-size 'cl-stack-icu:u-char)
                                            (1+ (max n 0))))
                 (setf n (cl-stack-icu:unum-format-double fmt (float value 1d0)
                                                          buf (1+ n) (null-pointer) err))
                 (cl-stack-icu:check-icu (mem-ref err :int) "unum-format-double")
                 (cl-stack-icu:u-chars-to-lisp buf n))))
        (cl-stack-icu:unum-close fmt)))))

(defmethod backend-format-number ((backend icu-backend) value &key locale style skeleton options)
  (declare (ignore backend skeleton options))
  (%format-double (or style :decimal) value locale))

(defmethod backend-format-percent ((backend icu-backend) value &key locale skeleton options)
  (declare (ignore backend skeleton options))
  (%format-double :percent value locale))

(defmethod backend-format-currency ((backend icu-backend) value currency &key locale skeleton options)
  (declare (ignore backend skeleton options))
  (with-foreign-object (err :int)
    (setf (mem-ref err :int) (%zero-error))
    (let ((fmt (cl-stack-icu:unum-open (%number-style :currency)
                                       (null-pointer) 0
                                       (%locale-string locale)
                                       (null-pointer) err)))
      (cl-stack-icu:check-icu (mem-ref err :int) "unum-open")
      (unwind-protect
           (call-with-uchars
            (string currency)
            (lambda (cur cur-len)
              (declare (ignore cur-len))
              (setf (mem-ref err :int) (%zero-error))
              (let ((n (cl-stack-icu:unum-format-double-currency
                        fmt (float value 1d0) cur
                        (null-pointer) 0 (null-pointer) err)))
                (setf (mem-ref err :int) (%zero-error))
                (with-foreign-pointer (buf (* (foreign-type-size 'cl-stack-icu:u-char)
                                              (1+ (max n 0))))
                  (setf n (cl-stack-icu:unum-format-double-currency
                           fmt (float value 1d0) cur buf (1+ n) (null-pointer) err))
                  (cl-stack-icu:check-icu (mem-ref err :int) "unum-format-double-currency")
                  (cl-stack-icu:u-chars-to-lisp buf n)))))
        (cl-stack-icu:unum-close fmt)))))

(defun %udate-ms (value)
  "VALUE is universal-time seconds or already UDate (ms). Heuristic: large → ms."
  (cond ((numberp value)
         (if (> (abs value) 1d12)
             (float value 1d0)
             (* (float value 1d0) 1000d0)))
        (t 0d0)))

(defmethod backend-format-date ((backend icu-backend) value &key locale style skeleton options)
  (declare (ignore backend skeleton options))
  (with-foreign-object (err :int)
    (setf (mem-ref err :int) (%zero-error))
    (let* ((ds (%date-style style))
           (fmt (cl-stack-icu:udat-open ds
                                        (foreign-enum-value 'cl-stack-icu:u-date-format-style :none)
                                        (%locale-string locale)
                                        (null-pointer) -1
                                        (null-pointer) 0 err)))
      (cl-stack-icu:check-icu (mem-ref err :int) "udat-open")
      (unwind-protect
           (progn
             (setf (mem-ref err :int) (%zero-error))
             (let ((n (cl-stack-icu:udat-format fmt (%udate-ms value)
                                                (null-pointer) 0 (null-pointer) err)))
               (setf (mem-ref err :int) (%zero-error))
               (with-foreign-pointer (buf (* (foreign-type-size 'cl-stack-icu:u-char)
                                            (1+ (max n 0))))
                 (setf n (cl-stack-icu:udat-format fmt (%udate-ms value)
                                                   buf (1+ n) (null-pointer) err))
                 (cl-stack-icu:check-icu (mem-ref err :int) "udat-format")
                 (cl-stack-icu:u-chars-to-lisp buf n))))
        (cl-stack-icu:udat-close fmt)))))

(defmethod backend-format-time ((backend icu-backend) value &key locale style skeleton options)
  (declare (ignore backend skeleton options))
  (with-foreign-object (err :int)
    (setf (mem-ref err :int) (%zero-error))
    (let* ((ts (%date-style style))
           (fmt (cl-stack-icu:udat-open
                 (foreign-enum-value 'cl-stack-icu:u-date-format-style :none)
                 ts (%locale-string locale)
                 (null-pointer) -1 (null-pointer) 0 err)))
      (cl-stack-icu:check-icu (mem-ref err :int) "udat-open")
      (unwind-protect
           (progn
             (setf (mem-ref err :int) (%zero-error))
             (let ((n (cl-stack-icu:udat-format fmt (%udate-ms value)
                                                (null-pointer) 0 (null-pointer) err)))
               (setf (mem-ref err :int) (%zero-error))
               (with-foreign-pointer (buf (* (foreign-type-size 'cl-stack-icu:u-char)
                                            (1+ (max n 0))))
                 (setf n (cl-stack-icu:udat-format fmt (%udate-ms value)
                                                   buf (1+ n) (null-pointer) err))
                 (cl-stack-icu:check-icu (mem-ref err :int) "udat-format")
                 (cl-stack-icu:u-chars-to-lisp buf n))))
        (cl-stack-icu:udat-close fmt)))))

(defmethod backend-format-datetime ((backend icu-backend) value &key locale date-style time-style
                                    skeleton options)
  (declare (ignore backend skeleton options))
  (with-foreign-object (err :int)
    (setf (mem-ref err :int) (%zero-error))
    (let ((fmt (cl-stack-icu:udat-open (%date-style date-style)
                                       (%date-style time-style)
                                       (%locale-string locale)
                                       (null-pointer) -1 (null-pointer) 0 err)))
      (cl-stack-icu:check-icu (mem-ref err :int) "udat-open")
      (unwind-protect
           (progn
             (setf (mem-ref err :int) (%zero-error))
             (let ((n (cl-stack-icu:udat-format fmt (%udate-ms value)
                                                (null-pointer) 0 (null-pointer) err)))
               (setf (mem-ref err :int) (%zero-error))
               (with-foreign-pointer (buf (* (foreign-type-size 'cl-stack-icu:u-char)
                                            (1+ (max n 0))))
                 (setf n (cl-stack-icu:udat-format fmt (%udate-ms value)
                                                   buf (1+ n) (null-pointer) err))
                 (cl-stack-icu:check-icu (mem-ref err :int) "udat-format")
                 (cl-stack-icu:u-chars-to-lisp buf n))))
        (cl-stack-icu:udat-close fmt)))))

(defmethod backend-format-relative-time ((backend icu-backend) value unit &key locale numeric options)
  (declare (ignore backend value unit locale numeric options))
  (error 'l10n-unsupported :capability :date
         :message "relative-time not in wave-1 FFI surface"))

(defmethod backend-format-list ((backend icu-backend) items &key locale type width options)
  (declare (ignore backend options))
  (let* ((type-e (foreign-enum-value 'cl-stack-icu:u-list-formatter-type
                                     (ecase (or type :and)
                                       (:and :and) (:or :or) (:units :units))))
         (width-e (foreign-enum-value 'cl-stack-icu:u-list-formatter-width
                                      (ecase (or width :wide)
                                        (:wide :wide) (:short :short) (:narrow :narrow))))
         (strings (mapcar #'string items))
         (count (length strings)))
    (with-foreign-object (err :int)
      (setf (mem-ref err :int) (%zero-error))
      (let ((fmt (cl-stack-icu:ulistfmt-open-for-type (%locale-string locale) type-e width-e err)))
        (cl-stack-icu:check-icu (mem-ref err :int) "ulistfmt-open")
        (unwind-protect
             ;; Convert each item to UChar*, then format.
             (let ((ptrs (cffi:foreign-alloc :pointer :count count))
                   (lens (cffi:foreign-alloc :int32 :count count))
                   (owned '()))
               (unwind-protect
                    (progn
                      (loop for s in strings for i from 0
                            do (call-with-uchars
                                s
                                (lambda (p n)
                                  (let ((copy (cffi:foreign-alloc 'cl-stack-icu:u-char :count n)))
                                    (dotimes (j n)
                                      (setf (mem-aref copy 'cl-stack-icu:u-char j)
                                            (mem-aref p 'cl-stack-icu:u-char j)))
                                    (push copy owned)
                                    (setf (mem-aref ptrs :pointer i) copy
                                          (mem-aref lens :int32 i) n)))))
                      (setf (mem-ref err :int) (%zero-error))
                      (let ((n (cl-stack-icu:ulistfmt-format fmt ptrs lens count
                                                             (null-pointer) 0 err)))
                        (setf (mem-ref err :int) (%zero-error))
                        (with-foreign-pointer (buf (* (foreign-type-size 'cl-stack-icu:u-char)
                                                      (1+ (max n 0))))
                          (setf n (cl-stack-icu:ulistfmt-format fmt ptrs lens count
                                                                buf (1+ n) err))
                          (cl-stack-icu:check-icu (mem-ref err :int) "ulistfmt-format")
                          (cl-stack-icu:u-chars-to-lisp buf n))))
                 (mapc #'cffi:foreign-free owned)
                 (cffi:foreign-free ptrs)
                 (cffi:foreign-free lens)))
          (cl-stack-icu:ulistfmt-close fmt))))))

(defmethod backend-parse-number ((backend icu-backend) string &key locale style options)
  (declare (ignore backend options))
  (with-foreign-object (err :int)
    (setf (mem-ref err :int) (%zero-error))
    (let ((fmt (cl-stack-icu:unum-open (%number-style (or style :decimal))
                                       (null-pointer) 0
                                       (%locale-string locale)
                                       (null-pointer) err)))
      (cl-stack-icu:check-icu (mem-ref err :int) "unum-open")
      (unwind-protect
           (call-with-uchars
            string
            (lambda (src src-len)
              (declare (ignore src-len))
              (setf (mem-ref err :int) (%zero-error))
              (let ((v (cl-stack-icu:unum-parse-double fmt src -1 (null-pointer) err)))
                (cl-stack-icu:check-icu (mem-ref err :int) "unum-parse-double")
                v)))
        (cl-stack-icu:unum-close fmt)))))

(defmethod backend-parse-date ((backend icu-backend) string &key locale style skeleton options)
  (declare (ignore backend skeleton options))
  (with-foreign-object (err :int)
    (setf (mem-ref err :int) (%zero-error))
    (let ((fmt (cl-stack-icu:udat-open (%date-style style) (%date-style style)
                                       (%locale-string locale)
                                       (null-pointer) -1 (null-pointer) 0 err)))
      (cl-stack-icu:check-icu (mem-ref err :int) "udat-open")
      (unwind-protect
           (call-with-uchars
            string
            (lambda (src src-len)
              (setf (mem-ref err :int) (%zero-error))
              (let ((ms (cl-stack-icu:udat-parse fmt src src-len (null-pointer) err)))
                (cl-stack-icu:check-icu (mem-ref err :int) "udat-parse")
                (/ ms 1000d0))))
        (cl-stack-icu:udat-close fmt)))))
