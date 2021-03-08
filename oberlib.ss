;;; -*- Gerbil -*-
;;; © ober
;;; my utils

(import
  :gerbil/gambit
  :gerbil/gambit/bits
  :gerbil/gambit/exact
  :gerbil/gambit/ports
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
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

(def JSON (getenv "JSON" #f))
(setenv "GAMBOPT" "-:tE8,f8,-8,h2097152")

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
     (display-exception e))))

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

(def (print-curl-headers headers)
  "Print function for headers passed to print-curl"
  (let ((results [])
        (final ""))
    (when (list? headers)
      (for (header headers)
        (set! results (cons (format " -H \'~a: ~a\'" (car header) (cdr header)) results)))
      (set! final (append-strings results)))
    final))

(def (print-curl type uri headers data)
  (let ((heads (print-curl-headers headers)))
    (cond
     ((equal? type 'get)
      (if data
        (displayln (format "curl -X GET ~a ~a" heads uri))
        (displayln (format "curl -X GET ~a -d \'~a\' ~a" heads data uri))))
     ((equal? type 'put)
      (displayln (format "curl -X PUT ~a -d \'~a\' ~a" heads data uri)))
     ((equal? type 'post)
      (write-string-to-file "data.txt" data)
      (displayln (format "curl -k -X POST ~a -d@data.txt \'~a\'" heads uri)))
     ((equal? type 'delete)
      (displayln (format "curl -X DELETE ~a ~a" heads uri)))
     (else
      (displayln "unknown format " type)))))

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
  (if (getenv "use_curl" #f)
    (print-curl type uri headers data)
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
         (when JSON
           (displayln text)
           (exit 0))
         (if (success? status)
           [ #t (from-json text) ]
           [ #f (format "Error: got ~a on request. text: ~a~%" status text) ])))
     (catch (os-exception? e)
       (when DEBUG
         (displayln "procedure: " (os-exception-procedure e))
         (displayln "arguments: " (os-exception-arguments e))
         (displayln "code: " (os-exception-code e))
         (displayln "message: " (os-exception-message e))))
     (catch (e)
       (display-exception e)))))

(def (rest-call-get uri headers)
  (http-get uri headers: headers))

(def (rest-call-post uri headers data)
  (http-post uri headers: headers data: data))

(def (rest-call-put uri headers data)
  (http-put uri headers: headers data: data))

(def (rest-call-delete uri headers)
  (http-delete uri headers: headers))

(def (do-post uri headers data)
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

(def (make-format-safe str)
  "Replace all ~ to ~~ except [~ to be safe for format use"
  (unless (string? str)
    str)
  (let ((regy (pregexp "(?:[^\\[])(\\~+)")))
    (pregexp-replace* regy str " ∼")))

(def (hash-interpol re delim str hsh fmt)
  "Given a RE, replace all instances in str with val from key matching RE"
  ;;(displayln "re: " re " delim: " delim " str: " str " fmt: " fmt)
  (unless (and
            (string? str)
            (table? hsh)
            re)
    str)
  (let* ((regy (pregexp re))
         (vars (remove-bad-matches (match-regexp regy str) delim))
         (newstr (pregexp-replace* regy (make-format-safe str) fmt))
         (set-vars []))
    (for (var vars)
      (let ((val (hash-get hsh var)))
        (if (not val)
          (error "Error: Variable " var " is used in the template, but not defined in the hash")
          (set! set-vars (cons val set-vars)))))
    (dp (format "hash-interpol: string: |~a| set-vars: |~a| newstr: |~a|" str set-vars newstr))
    (apply format newstr set-vars)))

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
            (error "Error: Variable " var " is used in the template, but not defined in the environment")
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

(def (any->int num)
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
    (if (string? safe-word)
      safe-word
      str)))

(def (get-if-set-b64 var alt)
  "Return the value of an env var if it is set, decoded from b64, else return alt"
  (let ((val (getenv var #f)))
    (if val
      (bytes->string (base64-decode val))
      alt)))

(def (pi item)
  (present-item item))

(def (present-item item)
  "Given a random object, print it out to the stdout"
  (cond
   ((table? item)
    (displayln (hash->string item)))
   ((or (string? item) (list? item) (number? item))
    (displayln item))
   ((eof-object? item)
    (displayln "eof"))
   ((void? item)
    (displayln "Null"))
   ((boolean? item)
    (if item
      (displayln "True")
      (displayln "False")))
   (else
    (displayln "present-item: unknown:" item))))

(def (find-files path
		 (pred? true)
		 recurse?: (recurse? true)
		 follow-symlinks?: (follow-symlinks? #f))
  (with-list-builder
   (collect!)
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

(defalias λ lambda)

(defrules ignore-errors ()
  ((_ form ...) (with-catch (λ (_) #f) (λ () form ...))))

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

(def (cache-or-run cache-file expiration process)
  "Given a procedure, check to see if cache exists within expiration time
   Return cached info if under expiration time.
   Otherwise, execute thunk/process and write to cache file.
   Returning data"
  (dp (present-item cache-file))
  (let* ((results #f)
         (cfe (file-exists? cache-file))
         (ms (when cfe (modified-since? cache-file expiration))))
    (if (and cfe
             ms)
      (begin
        (dp "cache-or-run: cache hit!")
        (set! results (read-obj-from-file cache-file)))
      (begin
        (dp "cache-or-run: cache miss :[")
        (set! results (eval process))
        (write-obj-to-file cache-file results)))
    results))

(def (write-obj-to-file out-file obj)
  "Serialize object to a file"
  (with-output-to-file [ path: out-file create: 'maybe truncate: #t ]
    (lambda ()
      (write-string (base64-encode (object->u8vector obj))))))

(def (write-string-to-file out-file str)
  "Write the contents of str to out-file"
  (with-output-to-file [ path: out-file create: 'maybe truncate: #t ]
    (lambda ()
      (write-string str))))

(def (read-obj-from-file in-file)
  "Serialize object to a file"
  (try
   (u8vector->object
    (base64-decode
     (read-file-string in-file)))
   (catch (e)
     (display-exception e))))

(def (modified-since? file secs-ago)
  "Check file mtime and determine if file is older than secs-ago"
  (if (file-exists? file)
    (let* ((now (float->int (time->seconds (builtin-current-time))))
           (mtime (time->seconds (file-info-last-modification-time (file-info file))))
           (diff (- now mtime)))
      (if (< diff secs-ago)
        #t
        #f))
    #f))

(def (rekey-sym hsh)
  "Convert all keys from strings to symbols, nondestructively"
  (unless (table? hsh)
    (error "hsh is not table." (type-of hsh)))
  (let (sym-hsh (hash))
    (hash-for-each
     (lambda (k v)
       (hash-put! sym-hsh (string->symbol k) v))
     hsh)
    sym-hsh))

(def (lines-to-spaces paragraph)
  "Convert newlines to spaces"
  (if (string? paragraph)
    (pregexp-replace* (string #\newline) paragraph (string #\space))
    paragraph))

(def (yon bool)
  "Return Yes, or No, based on bool"
  (if bool
    "Yes"
    "No"))
