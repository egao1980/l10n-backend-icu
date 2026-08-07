(defpackage #:l10n-backend-icu
  (:use #:cl #:l10n-protocol)
  (:export #:icu-backend
           #:use-icu-backend
           #:*icu-backend*))

(in-package #:l10n-backend-icu)
