(in-package #:l10n-backend-icu)

(defun %zero-error ()
  (foreign-enum-value 'cl-stack-icu:u-error-code :zero-error))

(defun %locale-string (locale)
  (cond ((null locale) "")
        ((stringp locale) locale)
        (t (princ-to-string locale))))

(defun call-with-uchars (string fn)
  (with-foreign-string (utf8 (string string) :encoding :utf-8)
    (with-foreign-objects ((err :int) (needed :int32))
      (setf (mem-ref err :int) (%zero-error))
      (cl-stack-icu:u-str-from-utf8 (null-pointer) 0 needed utf8 -1 err)
      (let ((n (mem-ref needed :int32)))
        (setf (mem-ref err :int) (%zero-error))
        (with-foreign-pointer (buf (* (foreign-type-size 'cl-stack-icu:u-char) (1+ (max n 0))))
          (cl-stack-icu:u-str-from-utf8 buf (1+ n) needed utf8 -1 err)
          (cl-stack-icu:check-icu (mem-ref err :int) "u-str-from-utf8")
          (funcall fn buf (mem-ref needed :int32)))))))

(defun %strength-enum (strength)
  (foreign-enum-value 'cl-stack-icu:u-col-attribute-value
                      (ecase strength
                        (:primary :primary)
                        (:secondary :secondary)
                        (:tertiary :tertiary)
                        (:quaternary :quaternary)
                        (:identical :identical)
                        (:default :default-strength))))
