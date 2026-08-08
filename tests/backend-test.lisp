(in-package #:l10n-backend-icu/tests)

(deftest backend-installed
  (ok (typep *l10n-backend* 'icu-backend))
  (ok (member :collate (backend-capabilities *l10n-backend*)))
  (ok (member :number (backend-capabilities *l10n-backend*))))

(deftest collate-en
  (ok (minusp (collate "a" "b" :locale "en")))
  (ok (zerop (collate "a" "a" :locale "en"))))

(deftest format-number-en
  (let ((s (format-number 1234.5d0 :locale "en_US")))
    (ok (search "1" s))))

(deftest locale-case-tr
  ;; Turkish I/ı — locale-sensitive
  (ok (string= (locale-downcase "I" :locale "en") "i")))

(deftest format-list-and
  (let ((s (format-list '("a" "b" "c") :locale "en" :type :and)))
    (ok (search "a" s))
    (ok (search "c" s))))
