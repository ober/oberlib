#!/usr/bin/env gxi

(import :std/iter
        :std/format
        :std/pregexp
        :std/sugar
        :std/db/leveldb
        :std/db/lmdb
        :std/generic
        "~/src/oberlib/oberlib.ss")

(def datums [["alpha" secret:]
             ["romeo" 'top]
             ["charlie" 8174]
             ["xray" "umbra"]])

(def (test-lmdb dir)
  (displayln "<--- Lmdb")
  (lmdb-db-open dir)
  (for (datum datums)
    (with ([ key value ] datum)
      (lmdb-db-put key value)))
  (for (datum datums)
    (with ([ key value ] datum)
      (let (fetched-value (lmdb-db-get key))
        (if (equal? value fetched-value)
          (display "OK: ")
          (display "FAIL: "))
        (displayln (format "Key: ~a Value: ~a:~a Fetched value: ~a" key (type-of value) value fetched-value)))))
  ;; (for (datum datums)
  ;;   (with ([ key value ] datum)
  ;;     (db-delete key)))
  ;; (for (datum datums)
  ;;   (with ([ key value ] datum)
  ;;     (let (fetched-value (lmdb-db-get key))
  ;;       (if (equal? value fetched-value)
  ;;         (display "OK: ")
  ;;         (display "FAIL: "))
  ;;       (displayln (format "Key: ~a Value: ~a:~a Fetched value: ~a" key (type-of value) value fetched-value))))))
)

(def (test-leveldb dir)
  (displayln "<---- LevelDB")
  (leveldb-db-open dir)
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
    (displayln (format "Update: value: ~a origin: ~a" fetched-value value)))
  ;; batch
  (leveldb-db-batch "a" "is for apple")
  (leveldb-db-write)
  (let ((fetched-value (leveldb-db-get "a")))
    (if (equal? fetched-value "is for apple")
      (display "OK: ")
      (display "FAIL: "))
    (displayln "Val: a fetched: " fetched-value " original: is for apple")))

(def (test-db-generics type dir)
  (let ((key "omg")
        (val "it works")
        (new-value "this is a new value"))
    (db-open type dir)
    ;; put/get
    (db-put key val)
    (let ((fetched-value (db-get key)))
      (if (equal? val fetched-value)
        (display "OK: ")
        (display "FAIL: "))
      (displayln type ": key: " key " val: " val " fetched: " fetched-value))
    ;; update
    (db-update key new-value)
    (let ((fetched-value (db-get key)))
      (if (equal? new-value fetched-value)
        (display "OK: ")
        (display "FAIL: "))
      (displayln type ": key: " key " new-value: " new-value " fetched: " fetched-value))
    ))

(def (test-hash-interpol-at-names)
  (let* ((re "(?:^|\\s)@([a-zA-Z0-9]*)")
         (delim "@")
         (fmt " ~a")
         (str "@barbaz says this is a test for @foobar and @foobaz but not for jeff@example.com")
         (hsh (hash ("foobar" "John Doe")("foobaz" "Jane Doe")("barbaz" "Jeff Bono")))
         (expected " Jeff Bono says this is a test for John Doe and Jane Doe but not for jeff@example.com")
         (results (hash-interpol re delim str hsh fmt)))
    (if (string=? expected results)
      (displayln "test-hash-interpol-at-names: OK")
      (displayln "test-hash-interpol-at-names: FAIL! " results))))

(test-hash-interpol-at-names)

  ;; Run 'em!
;; (test-lmdb "/tmp/lmdb")
;; (test-leveldb "/tmp/leveldb")
;; (displayln "<---- Db generic")
;; (test-db-generics 'lmdb "/tmp/generics.lmdb")
;; (test-db-generics 'leveldb "/tmp/generics.leveldb")
