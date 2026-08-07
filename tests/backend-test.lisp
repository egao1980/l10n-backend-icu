(in-package #:l10n-backend-icu/tests)

(deftest system-loads
  (ok (asdf:find-system "l10n-backend-icu")))

(deftest use-icu-backend-installs
  (let ((backend (use-icu-backend)))
    (ok (typep backend 'icu-backend))
    (ok (eq *l10n-backend* backend))
    (ok (null (backend-capabilities backend)))))
