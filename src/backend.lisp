(in-package #:l10n-backend-icu)

(defclass icu-backend (l10n-backend) ()
  (:documentation "l10n-protocol backend over cl-stack-icu (ICU4C)."))

(defvar *icu-backend* nil)

(defmethod backend-capabilities ((backend icu-backend))
  '(:collate :number :date :currency :list :locale-case))

(defun use-icu-backend (&optional (backend (or *icu-backend*
                                              (setf *icu-backend*
                                                    (make-instance 'icu-backend)))))
  (use-l10n-backend backend)
  backend)

(use-icu-backend)
