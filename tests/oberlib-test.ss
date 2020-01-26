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
  (displayln "<--- Lmdb")
  (lmdb-db-open "/tmp/meowmix")
  (for (datum datums)
    (with ([ key value ] datum)
      (lmdb-db-put key value)))
  (for (datum datums)
    (with ([ key value ] datum)
      (let (fetched-value (lmdb-db-get key))
        (if (equal? value fetched-value)
          (display "OK: ")
          (display "FAIL: "))
        (displayln (format "Key: ~a Value: ~a:~a Fetched value: ~a" key (type-of value) value fetched-value))))))

(def (test-leveldb)
  (displayln "<---- LevelDB")
  (leveldb-db-open "/tmp/meoxmix2")
  (for (datum datums)
    (with ([ key value ] datum)
      (leveldb-db-put key value)))

  (for (datum datums)
    (with ([ key value ] datum)
      (let ((fetched-value (leveldb-db-get key)))
        (if (equal? value fetched-value)
          (display "OK: ")
          (display "FAIL: "))
        (displayln (format "Key: ~a Value: ~a:~a Fetched value: ~a" key (type-of value) value fetched-value)))))

  (let* ((key "update-test")
         (value "original value")
         (update "new value")
         (_ (leveldb-db-put key value))
         (_ (leveldb-db-update key update))
         (fetched-value (leveldb-db-get key)))
    (if (equal? update fetched-value)
      (display "OK: ")
      (display "FAIL: "))
    (displayln (format "Update: value: ~a" fetched-value))))

(def (test-db-generics)
  (let ((dir "/tmp/genericsdb-test2/"))
    ;;    (create-directory* dir)
    (with ([ env db ] (db-open lmdb: dir))
      (db-put "omg" "itworks"))))

;; Run 'em!
;;(test-db-generics)
(test-lmdb)
(test-leveldb)
