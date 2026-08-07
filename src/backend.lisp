(in-package #:l10n-backend-icu)

(defclass icu-backend (l10n-backend) ()
  (:documentation "l10n-protocol backend over ICU4C (scaffold — CFFI bindings TODO)."))

(defvar *icu-backend* nil)

(defmethod backend-capabilities ((backend icu-backend))
  ;; Planned: :collate :number :date :currency :list :locale-case
  '())

(defun use-icu-backend (&optional (backend (or *icu-backend*
                                              (setf *icu-backend*
                                                    (make-instance 'icu-backend)))))
  "Install ICU backend as *L10N-BACKEND*. Returns BACKEND."
  (use-l10n-backend backend)
  backend)
