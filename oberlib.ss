;;; -*- Gerbil -*-
;;; © ober
;;; my utils

(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/db/dbi
  :std/db/postgresql
  :std/db/postgresql-driver
  :std/db/lmdb
  :std/db/leveldb
  :scheme/base
  :std/debug/heap
  :std/iter
  :std/error
  :std/format
  :std/generic
  :std/generic/dispatch
  :std/misc/channel
  :std/misc/list
  :std/misc/ports
  :std/net/address
  :std/net/request
  :std/net/uri
  :std/pregexp
  :std/srfi/1
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  :std/text/zlib
  :std/xml/ssax)

(export #t)

(import (rename-in :gerbil/gambit/os (current-time builtin-current-time)))
(import (rename-in :gerbil/gambit/os (time mytime)))
(declare (not optimize-dead-definitions))


(def (strip-both string)
  "Safely strip leading, and trailing whitespace"
  (if (and (string? string)
           (> (string-length string) 1))
    (pregexp-replace "\ +$" (pregexp-replace "^\ +" string "") "")
    string))

(def (format-string-size string size)
  (unless (string? string)
    (set! string (format "~a" string)))
  (let* ((string (strip-both string))
         (our-size (string-length string))
         (delta (if (> size our-size)
                  (- size our-size)
                  0)))
    ;;    (displayln "fss: delta: " delta " string: " string " our-size: " our-size " size: " size)
    (format " ~a~a" string (make-string delta #\space))))

(def (dp msg)
  (when DEBUG
    (displayln msg)))

(def DEBUG (getenv "DEBUG" #f))

(def (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))

(def (float->int num)
  (inexact->exact
   (round num)))

(def (print-date date)
  (date->string date "~c"))

(def (from-json json)
  (try
   (with-input-from-string json read-json)
   (catch (e)
     (displayln "error parsing json " e))))

(def (epoch->date epoch)
  (cond
   ((string? epoch)
    (time-utc->date (make-time time-utc 0 (string->number epoch))))
   ((flonum? epoch)
    (time-utc->date (make-time time-utc 0 (float->int epoch))))
   ((fixnum? epoch)
    (time-utc->date (make-time time-utc 0 epoch)))))

(def (date->epoch mydate)
  (string->number (date->string (string->date mydate "~Y-~m-~d ~H:~M:~S") "~s")))

(def (strip-quotes str)
  (pregexp-replace*
   "\""
   str
   ""))

(def (success? status)
  (and (>= status 200) (<= status 299)))

(def (do-delete uri headers)
  (let* ((reply (http-delete uri
                             headers: headers))
         (status (request-status reply))
         (text (request-text reply)))
    (print-curl "delete" uri "" "")
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (hash->string h)
  (let ((results []))
    (if (table? h)
      (begin
        (hash-for-each
         (lambda (k v)
           (set! results (append results [ (format " ~a->" k) (format "~a   " v)])))
         h)
        (append-strings results))
      "N/A")))

(defalias hash->str hash->string)

(def (print-curl type uri headers data)
  ;;(displayln headers)
  (let ((heads "Content-type: application/json")
        (do-curl (getenv "DEBUG" #f)))
    (when do-curl
      (cond
       ((string=? type "get")
        (if (string=? "" data)
          (displayln (format "curl -X GET -H \'~a\' ~a" heads uri))
          (displayln (format "curl -X GET -H \'~a\' -d \'~a\' ~a" heads data uri))))
       ((string=? type "put")
        (displayln (format "curl -X PUT -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "post")
        (displayln (format "curl -X POST -H \'~a\' -d \'~a\' ~a" heads data uri)))
       ((string=? type "delete")
        (displayln (format "curl -X DELETE -H \'~a\' -d \'~a\' ~a" heads data uri)))
       (else
        (displayln "unknown format " type))))))

(def (do-get-generic uri headers)
  (let* ((reply (http-get uri
                          headers: headers))
         (status (request-status reply))
         (text (request-text reply)))
    (print-curl "get" uri "" "")
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (rest-call type uri headers (data #f))
  "Wrapper for all http queries that should return json on success.
   We return a list of OK?: #t/#f and results: object"
  (try
   (let ((reply
          (cond
           ((equal? type 'get)
            (rest-call-get uri headers))
           ((equal? type 'post)
            (rest-call-post uri headers data))
           ((equal? type 'put)
            (rest-call-put uri headers data))
           ((equal? type 'delete)
            (rest-call-delete uri headers)))))
     (let ((status (request-status reply))
           (text (request-text reply)))
       (if (success? status)
         [ #t (from-json text) ]
         [ #f (format "Error: got ~a on request. text: ~a~%" status text) ])))
   (catch (e)
     (display-exception e))))

(def (rest-call-get uri headers)
  (http-get uri headers: headers))

(def (rest-call-post uri headers data)
  (http-post uri headers: headers data: data))

(def (rest-call-put uri headers data)
  (http-put uri headers: headers data: data))

(def (rest-call-delete uri headers)
  (http-delete uri headers: headers))

(def (do-post uri headers data)
  (dp (print-curl "post" uri headers data))
  (try
   (let* ((reply (http-post uri headers: headers data: data))
          (status (request-status reply))
          (text (request-text reply)))
     (if (success? status)
       text
       (displayln (format "Failure on post. Status:~a Text:~a~%" status text))))
   (catch (e)
     (display-exception e))))

(def (do-get uri)
  (print-curl "get" uri "" "")
  (let* ((reply (http-get uri))
         (status (request-status reply))
         (text (request-text reply)))
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))

(def (do-post-generic uri headers data)
  (let* ((reply (http-post uri
                           headers: headers
                           data: data))
         (status (request-status reply))
         (text (request-text reply)))
    (dp (print-curl "post" uri headers data))
    (if (success? status)
      text
      (displayln (format "Error: Failure on a post. got ~a text: ~a~%" status text)))))

(def (do-put uri headers data)
  (dp (print-curl "put" uri headers data))
  (let* ((reply (http-put uri
                          headers: headers
                          data: data)))
    reply))

(def (remove-bad-matches vars omit)
  (let ((goodies []))
    (for (var vars)
      (unless (string-contains var omit)
        (set! goodies (flatten (cons var goodies)))))
    (reverse goodies)))

(def (interpol str)
  (displayln (interpol-from-env str)))

(def (interpol-from-env str)
  (if (not (string? str))
    str
    (let* ((ruby (pregexp "#\\{([a-zA-Z0-9_-]*)\\}"))
           (vars (remove-bad-matches (match-regexp ruby str) "#"))
           (newstr (pregexp-replace* ruby str "~a"))
           (set-vars []))

      (for (var vars)
        (let ((val (getenv var #f)))
          (if (not val)
            (begin
              (displayln "Error: Variable " var " is used in the template, but not defined in the environment")
              (exit 2))
            (set! set-vars (cons val set-vars)))))
      (dp (format "interpol-from-env: string: ~a set-vars: ~a newstr: ~a" str set-vars newstr))
      (apply format newstr set-vars))))

(def (match-regexp pat str . opt-args)
  "Like pregexp-match but for all matches til end of str"
  (let ((n (string-length str))
        (ix-prs []))
    (let lp ((start 0))
      (let* ((pp (pregexp-match-positions pat str start n))
             (ix-pr (pregexp-match pat str start n)))
        (if ix-pr
          (let ((pos (+ 1 (cdar pp))))
            (set! ix-prs (flatten (cons ix-pr ix-prs)))
            (if (< pos n)
              (lp pos)
              ix-prs))
          (reverse ix-prs))))))

(def (style-output infos (style "org-mode"))
  (when (list? infos)
    (let* ((sizes (hash))
           (data (reverse infos))
           (header (car data))
           (rows (cdr data))
           (header-sep "|"))
      (for (head header)
        (unless (string? head) (displayln "head is not string: " head) (exit 2))
        (hash-put! sizes head (string-length head)))
      (for (row rows)
        (let (count 0)
          (for (column row)
            (let* ((col-name (nth count header))
                   (current-size (hash-ref sizes col-name))
                   (this-size (if (string? column) (string-length column) (string-length (format "~a" column)))))
              (when (> this-size current-size)
                (hash-put! sizes col-name this-size))
              ;;		      (displayln "colname: " col-name " col: " count " current-size: " current-size " this-size: " this-size " column: " column)
              (set! count (1+ count))))))

      (cond
       ((string=? style "org-mode")
        (set! header-sep "| "))
       ((string=? style "confluence-markdown")
        (set! header-sep "||")))

      (for (head header)
        (display (format "~a~a" header-sep (format-string-size head (hash-get sizes head)))))

      ;; print header
      (displayln header-sep)
      (let ((count 0))
        (for (head header)
          (let ((sep (if (= count 0) "|" "+")))
            (display (format "~a~a" sep (make-string (+ 2 (hash-get sizes (nth count header))) #\-))))
          (set! count (1+ count))))
      (displayln "|")

      (for (row rows)
        (let (count 0)
          (for (col row)
            (display (format "|~a " (format-string-size col (hash-ref sizes (nth count header)))))
            (set! count (1+ count))))
        (displayln "|"))
      )))

(def (print-header style header)
  (cond
   ((string=? style "org-mode")
    (displayln "| " (string-join header " | ") " |")
    (displayln "|-|"))
   ((string=? style "confluence-markdown")
    (displayln "|| " (string-join header "||") "||"))
   (else
    (displayln "Unknown format: " style))))

(def (print-row style data)
  (if (list? data)
    (cond
     ((string=? style "org-mode")
      (org-mode-print-row data))
     ((string=? style "confluence-markdown")
      (displayln "|" (string-join data "|") "|"))
     (else
      (displayln "Unknown format! " style)))))

(def (org-mode-print-row data)
  (when (list? data)
    (for (datum data)
      (printf "| ~a " datum))
    (displayln "|")))

(def (date->custom dt)
  (date->string (string->date dt "~Y-~m-~dT~H:~M:~S~z") "~a ~b ~d ~Y"))

(def (resolve-ipv4 host)
  (let* ((host-info (host-info-addresses (host-info host))))
    (dp (format "host-info: ~a type:~a" host-info (type-of host-info)))
    (ip4-address->string
     (car host-info))))

(def (make-basic-auth user password)
  (format "Basic ~a"
          (base64-encode
           (string->utf8 (format "~a:~a" user password)))))

(def (def-num num)
  (if (string? num)
    (string->number num)
    num))

(def (sis item)
  (if item
    item
    "N/A"))

;; read-password provided by @feeley
(def (raw-mode tty)
  (##tty-mode-set! tty
                   #f ;; input-allow-special
                   #f ;; input-echo
                   #t ;; input-raw
                   #t ;; output-raw
                   0)) ;; speed

(def (cooked-mode tty)
  (##tty-mode-set! tty
                   #t ;; input-allow-special
                   #t ;; input-echo
                   #f ;; input-raw
                   #f ;; output-raw
                   0)) ;; speed

(def (read-password tty)
  (raw-mode tty)
  (let loop ((chars []))
    (let ((c (read-char tty)))
      (cond ((or (eof-object? c)
                 (char=? c #\return)
                 (char=? c #\newline))
             (cooked-mode tty)
             (display "\n" tty)
             (list->string (reverse chars)))
            ((or (char=? c #\backspace)
                 (char=? c #\delete))
             (if (pair? chars)
               (begin
                 (display "\b \b" tty)
                 (loop (cdr chars)))
               (loop chars)))
            (else
             (display "*" tty)
             (loop (cons c chars)))))))

(def (filter-headers headers fields)
  (let ((ours headers))
    (for (header headers)
      (unless (member header fields)
        (displayln "removing " header)
        (delete! header ours)))
    ours))

(def (filter-row name value headers)
  (when (member name headers)
    value))

(def (filter-row-hash row fields)
  (let ((final []))
    (for (field fields)
      (let ((value (hash-get row field)))
        (if value
          (set! final (cons value final))
          (dp (format "Field ~a was requested but not found in fields hash" field)))))
    (reverse final)))

(def (web-encode str)
  "Interface to encode a string for uri encodings, and stuff"
  (def (write-uri-encoded str encoding)
    (def (write-hex n)
      (write-char (##string-ref "0123456789ABCDEF" n)))

    (let* ((utf8 (string->utf8 str))
           (len  (u8vector-length utf8)))
      (let lp ((n 0))
        (when (##fx< n len)
          (let (byte (##u8vector-ref utf8 n))
            (cond
             ((##vector-ref encoding byte) => write-char)
             (else
              (write-char #\%)
              (write-hex (##fxand (##fxarithmetic-shift byte -4) #xf))
              (write-hex (##fxand byte #xf))))
            (lp (##fx+ n 1)))))))

  (let* ((uri-encoding (make-uri-encoding-table uri-unreserved-chars))
         (safe-word (write-uri-encoded str uri-encoding)))
    safe-word))

(def (get-if-set-b64 var alt)
  "Return the value of an env var if it is set, decoded from b64, else return alt"
  (let ((val (getenv var #f)))
    (if val
      (bytes->string (base64-decode val))
      alt)))

(def (default-headers basic)
  [
   ["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]
   ["Authorization" :: basic ]
   ])

(def (present-item item)
  "Given a random object, print it out to the stdout"
  (cond
   ((table? item)
    (displayln (hash->string item)))
   ((or (string? item) (list? item))
    (displayln item))
   (else
    (displayln "present-item: unknown:" item))))


(def (find-files path
		 (pred? true)
		 recurse?: (recurse? true)
		 follow-symlinks?: (follow-symlinks? #f))
  (with-list-builder (collect!)
		     (walk-filesystem-tree! path
					    (λ (file) (when (pred? file) (collect! file)))
					    recurse?: recurse?
					    follow-symlinks?: follow-symlinks?)))

(def (walk-filesystem-tree!
      path
      visit
      recurse?: (recurse? true)
      follow-symlinks?: (follow-symlinks? #f))
  (visit path)
  (when (and (ignore-errors (path-is-directory? path follow-symlinks?))
	     (recurse? path))
    (for-each!
     (directory-files path)
     (λ (name) (walk-filesystem-tree!
		(path-expand name path) visit
		recurse?: recurse? follow-symlinks?: follow-symlinks?)))))

;;;; DB OPERATIONS
(def :db-db #!void)
(def :db-env #!void)
(def :db-type #!void)

(def (db-put key value)
  (cond
   ((equal? :db-type 'leveldb)
    (leveldb-db-put key value))
   ((equal? :db-type 'lmdb)
    (lmdb-db-put key value))
   ((void? :db-type)
    (displayln ":db-type is void. Failed to initialize db?"))
   (else
    (displayln "Unknown DB type " :db-type))))

(def (db-get key)
  (cond
   ((equal? :db-type 'leveldb)
    (leveldb-db-get key))
   ((equal? :db-type 'lmdb)
    (lmdb-db-get key))
   (else
    (displayln "Unknown DB type " :db-type))))

(def (db-open type dir)
  (cond
   ((equal? type 'leveldb)
    (set! :db-type 'leveldb)
    (leveldb-db-open dir))
   ((equal? type 'lmdb)
    (set! :db-type 'lmdb)
    (lmdb-db-open dir))
   (else
    (displayln "Unknown :db-type: " type)
    (exit 2))))

;;(def db-dir (or (getenv "oberlibdb" #f) ".")) ;;(format "~a/kunabi-db/" (user-info-home (user-info (user-name))))))

(def (db-close)
  (cond
   ((equal? :db-type 'lmdb)
    (lmdb-db-close))
   ((equal? :db-type 'leveldb)
    (leveldb-db-close))
   (else
    (displayln "Unknown :db-type: " :db-type)
    (exit 2))))

(def (db-key? key)
  (cond
   ((equal? :db-type 'lmdb)
    (or (lmdb-db-get key) #f))
   (else
    (displayln "Unknown :db-type: " :db-type)
    (exit 2))))

(def (db-batch batch key value)
  (cond
   ((equal? :db-type 'lmdb)
    (lmdb-db-put key value))
   ((equal? :db-type 'leveldb)
    (leveldb-db-put key value))
   (else
    (displayln "Unknown :db-type: " :db-type)
    (exit 2))))

;; leveldb helpers
(def (leveldb-db-open dir)
  (set! :db-db (leveldb-open dir)))

(def (leveldb-db-close)
  (leveldb-close :db-db))

(def (leveldb-db-get key)
  (try
   (let* ((bytes (leveldb-get :db-db (format "~a" key)))
          (val (if (u8vector? bytes)
                 (u8vector->object bytes)
                 #f)))
     val)
   (catch (e)
     (raise e))))

(def (leveldb-db-put key val)
  (try
   (leveldb-put :db-db key (object->u8vector val))
   (catch (e)
     (raise e))))

(def (leveldb-db-update key val)
  (try
   (leveldb-db-put key val)
   (catch (e)
     (raise e))))

(def (leveldb-db-remove key)
  (try
   (leveldb-delete :db-db key)
   (catch (e)
     (raise e))))

;; lmdb specifics

(def (lmdb-db-open dir)
  "Return a db handle to env and name"
  (set! :db-env (lmdb-open dir))
  (set! :db-db (lmdb-open-db :db-env "ober")))

(def (lmdb-make-env dir)
  "lmdb needs an env to be passed for lmdb-open-db"
  (lmdb-open dir))

(def (lmdb-db-update key val)
  (let* ((txn (lmdb-txn-begin :db-env))
         (bytes (lmdb-get txn :db-db key))
         (current (if bytes
                    (call-with-input-u8vector (uncompress bytes) read-json)
                    #f))
         (new (if (table? current)
                (hash-put! current val #t)))
         (final (compress (call-with-output-u8vector [] (cut write-json new <>)))))
    ;;(bytes (call-with-output-u8vector [] (cut write-json val <>)))
    ;; (bytes (compress bytes))
    (try
     (lmdb-put txn :db-db key final)
     (lmdb-txn-commit txn)
     (catch (e)
       (lmdb-txn-abort txn)
       (raise e)))))

(def (lmdb-db-get key)
  (let (txn (lmdb-txn-begin :db-env))
    (try
     (let* ((bytes (lmdb-get txn :db-db key))
	    (val (if bytes
		   (call-with-input-u8vector (uncompress bytes) read-json)
		   #f)))
       (lmdb-txn-commit txn)
       val)
     (catch (e)
       (lmdb-txn-abort txn)
       ;;(display e)
       ;;(displayln "error kunabi-store-get: key:" key)
       (raise e)))))

(def (lmdb-db-put key val)
  (let* ((bytes (call-with-output-u8vector [] (cut write-json val <>)))
	 (bytes (compress bytes))
	 (txn (lmdb-txn-begin :db-env)))
    (try
     (lmdb-put txn :db-db key bytes)
     (lmdb-txn-commit txn)
     (catch (e)
       (lmdb-txn-abort txn)
       (raise e)))))

(def (lmdb-db-close)
  "noop"
  (display "noop"))

;;(def records (db-open :db-type))

;;(def env records)

(defalias λ lambda)

(defrules ignore-errors ()
  ((_ form ...) (with-catch (λ (_) #f) (λ () form ...))))

(def (for-each! list fun)
  (match list
    ([elem . more] (fun elem) (for-each! more fun))
    (_ (void))))

(defrules with-list-builder ()
  ((_ (c r) body1 body+ ...) (call-with-list-builder (λ (c r) body1 body+ ...)))
  ((_ (c) body1 body+ ...) (with-list-builder (c _) body1 body+ ...)))

(def (subpath top . sub-components)
  (path-expand (string-join sub-components "/") top))

(def (path-is-symlink? path)
  (equal? 'symbolic-link (file-info-type (file-info path #f))))

(def (path-is-not-symlink? path)
  (not (path-is-symlink? path)))

(def (path-is-file? path (follow-symlinks? #f))
  (equal? 'regular (file-info-type (file-info path follow-symlinks?))))

(def (path-is-directory? path (follow-symlinks? #f))
  (equal? 'directory (file-info-type (file-info path follow-symlinks?))))
