#!/usr/bin/env gxi

(import :std/iter
        :std/format
        :std/db/lmdb
        :std/generic
        "~/src/oberlib/oberlib.ss")

(def (test-lmdb)
  (let* ((datums [["alpha" secret:]
                 ["romeo" 'top]
                 ["charlie" 8174]
                 ["xray" "umbra"]])
        (env (lmdb-make-env "/tmp/test-lmdb"))
        (db (lmdb-open-db env "tests")))
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


(test-lmdb)
