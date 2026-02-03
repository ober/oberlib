#!/usr/bin/env gxi

(import :std/iter
        :std/format
        :std/pregexp
        :std/sugar
        :ober/oberlib)

(def (test-hash-interpol-at-names)
  (let* ((re "(?:^|\\s)@([a-zA-Z0-9]*)")
         (delim "@")
         (fmt " ~a")
         (str "@barbaz says this is a test for @foobar and @foobaz but not for jeff@example.com or for odd ~~ characters left over")
         (hsh (hash ("foobar" "John Doe")("foobaz" "Jane Doe")("barbaz" "Jeff Bono")))
         (expected " Jeff Bono says this is a test for John Doe and Jane Doe but not for jeff@example.com or for odd ∼ characters left over")
         (results (hash-interpol re delim str hsh fmt)))
    (if (string=? expected results)
      (displayln "test-hash-interpol-at-names: OK")
      (displayln "test-hash-interpol-at-names: FAIL! " results))))

(test-hash-interpol-at-names)
