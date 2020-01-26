#!/usr/bin/env gxi

(import :std/iter
        :std/format
        :std/db/lmdb
        :std/generic
        "~/src/oberlib/oberlib.ss")

(def datums [["alpha" secret:]
             ["romeo" 'top]
             ["charlie" 8174]
             ["xray" "umbra"]])

(def (test-lmdb)
  (let* ((env (lmdb-make-env "/tmp/test-lmdb"))
         (db (lmdb-db-open env "tests")))
    (for (datum datums)
      (with ([ key value ] datum)
        (lmdb-db-put env db key value)))
    (for (datum datums)
      (with ([ key value ] datum)
        (let (fetched-value (lmdb-db-get env db key))
          (if (equal? value fetched-value)
            (display "OK: ")
            (display "FAIL: "))
          (displayln (format "Key: ~a Value: ~a:~a Fetched value: ~a" key (type-of value) value fetched-value)))))))

(def (test-leveldb)
  (def env #f)
  (def db (leveldb-db-open (format "/tmp/test.db.~a" (random-integer (expt 2 32)))))

  (for (datum datums)
    (with ([ key value ] datum)
      (leveldb-db-put #f db key value)))

  (for (datum datums)
    (with ([ key value ] datum)
      (let ((fetched-value (leveldb-db-get env db key)))
        (if (equal? value fetched-value)
          (display "OK: ")
          (display "FAIL: "))
        (displayln (format "Key: ~a Value: ~a:~a Fetched value: ~a" key (type-of value) value fetched-value)))))

  (let* ((key "update-test")
        (value "original value")
        (update "new value")
        (_ (leveldb-db-put #f db key value))
        (_ (leveldb-db-update #f db key update))
        (fetched-value (leveldb-db-get #f db key)))
    (if (equal? update fetched-value)
      (display "OK: ")
      (display "FAIL: "))
    (displayln (format "Update: value: ~a" fetched-value))))


(test-lmdb)
(test-leveldb)
