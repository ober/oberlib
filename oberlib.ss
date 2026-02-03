;;; -*- Gerbil -*-
;;; © ober
;;; my utils

(import
  :gerbil/gambit
  :scheme/base
  :std/actor-v18/io
  :std/error
  :std/format
  :std/io
  :std/iter
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
  )

(export
  strip-both
  format-string-size
  nth
  float->int
  print-date
  from-json
  epoch->date
  date->epoch
  date->epoch2
  strip-quotes
  success?
  do-delete
  hash->string
  hash->str
  format-curl-headers
  print-curl
  format-curl-cmd
  do-get-generic
  rest-call
  rest-call-get
  rest-call-post
  rest-call-put
  rest-call-delete
  do-post
  do-get
  do-post-generic
  do-put
  remove-bad-matches
  interpol
  make-format-safe
  hash-interpol
  interpol-from-env
  match-regexp
  style-output
  compare-lst-car
  print-header
  print-row
  org-mode-print-row
  date->custom
  resolve-ipv4
  make-basic-auth
  any->int
  sis
  filter-headers
  filter-row
  filter-row-hash
  web-encode
  get-if-set-b64
  pi
  present-item
  find-files
  walk-filesystem-tree!
  subpath
  path-is-symlink?
  path-is-not-symlink?
  path-is-file?
  path-is-directory?
  cache-or-run
  cache-or-run-json
  write-obj-to-file
  write-string-to-file
  write-json-to-file
  read-obj-from-file
  read-json-from-file
  modified-since?
  my-ignore-errors
  rekey-sym
  lines-to-spaces
  yon
  marshal-value
  unmarshal-value
  my-json-object->string
  mixed-string-join
  def-num)

(def JSON (getenv "JSON" #f))

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
    (format "~a~a " string (make-string delta #\space))))

(def DEBUG (getenv "DEBUG" #f))

(def (dp msg)
  (when DEBUG
    (displayln msg)))

(def (nth n l)
  (if (or (>= n (length l)) (< n 0))
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
  (parameterize ((read-json-key-as-symbol? #t))
    (try
     (with-input-from-string json read-json)
     (catch (e)
       (display-exception e)))))

(def (epoch->date epoch)
  (cond
   ((string? epoch)
    (time-utc->date (make-time time-utc 0 (string->number epoch))))
   ((flonum? epoch)
    (time-utc->date (make-time time-utc 0 (float->int epoch))))
   ((fixnum? epoch)
    (time-utc->date (make-time time-utc 0 epoch)))
   (else
    (error "epoch->date: unsupported type" epoch))))

(def (date->epoch mydate)
  (string->number (date->string (string->date mydate "~Y-~m-~d ~H:~M:~S") "~s")))

(def (date->epoch2 mydate)
  (string->number (date->string (string->date mydate "~Y-~m-~dT~H:~M:~SZ") "~s")))

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
         (headers (request-headers reply))
         (text (request-text reply)))
    (print-curl 'delete uri "" "")
    (if (success? status)
      text
      (begin
        (displayln (format "Error: got ~a on request. text: ~a~%" status text))
        #f))))

(def (hash->string h)
  (let ((results []))
    (if (hash-table? h)
      (begin
        (hash-for-each
         (lambda (k v)
           (set! results (append results [ (format " ~a->" k) (format "~a   " v)])))
         h)
        (string-concatenate results))
      "N/A")))

(defalias hash->str hash->string)

(def (format-curl-headers headers)
  "Print function for headers passed to print-curl"
  (let ((results [])
        (final ""))
    (when (list? headers)
      (for (header headers)
        (set! results (cons (format " -H \'~a: ~a\'" (car header) (cdr header)) results)))
      (set! final (string-concatenate results)))
    final))

(def (print-curl type uri headers data)
  "Print curl equivalent for debugging. Does NOT execute shell commands.
   SECURITY: Shell command execution removed to prevent command injection.
   Use rest-call for actual HTTP requests."
  (let ((curl (format-curl-cmd type uri headers data)))
    (when (getenv "print_curl" #f)
      (displayln curl))
    ;; Return #f to indicate this is debug-only, not for execution
    #f))

(def (format-curl-cmd type uri headers data)
  (let ((heads (format-curl-headers headers)))
    (cond
     ((equal? type 'get)
      (if data
        (format "curl -sS -k -X GET ~a -d '~a' ~a" heads data uri)
        (format "curl -sS -k -X GET ~a ~a" heads uri)))
     ((equal? type 'put)
      (format "curl -sS -k -X PUT ~a -d '~a' '~a'" heads data uri))
     ((equal? type 'post)
      (format "curl -sS -k -X POST ~a -d '~a' '~a'" heads data uri))
     ((equal? type 'delete)
      (format "curl -sS -k -X DELETE ~a ~a" heads uri))
     (else
      (format "unknown format: ~a" type)))))

(def (do-get-generic uri headers)
  (let* ((reply (http-get uri
                          headers: headers))
         (status (request-status reply))
         (headers (request-headers reply))
         (text (request-text reply)))
    (print-curl 'get uri "" "")
    (if (success? status)
      text
      (begin
        (displayln (format "Error: got ~a on request. text: ~a~%" status text))
        #f))))

(def (rest-call type uri headers (data #f) (retry 0))
  "Wrapper for all http queries that should return json on success.
   We return a list of OK?: #t/#f and results: object
   SECURITY: Always uses native HTTP functions, never shell commands."
  (dp (format "rest-call: type: ~a uri: ~a headers: ~a data: ~a retry: ~a" type uri headers data retry))
  ;; Print curl equivalent for debugging if requested (does not execute)
  (when (getenv "print_curl" #f)
    (displayln (format-curl-cmd type uri headers data)))
  (let lp ((count 0))
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
             (headers (request-headers reply))
             (text (request-text reply)))
         (when JSON
           (displayln text))
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
       (displayln "count: " count " retry: " retry)
       (if (< count retry)
         (begin
           (displayln "retry #" count)
           (lp (+ 1 count)))
         (error (format "Out of retries. Count: ~a error: ~a" count  e)))))))

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
          (headers (request-headers reply))
          (text (request-text reply)))
     (if (success? status)
       text
       (begin
         (displayln (format "Failure on post. Status:~a Text:~a~%" status text))
         #f)))
   (catch (e)
     (display-exception e)
     #f)))

(def (do-get uri)
  (let* ((reply (http-get uri))
         (status (request-status reply))
         (text (request-text reply)))
    (if (success? status)
      text
      (begin
        (displayln (format "Error: got ~a on request. text: ~a~%" status text))
        #f))))

(def (do-post-generic uri headers data)
  (let* ((reply (http-post uri
                           headers: headers
                           data: data))
         (status (request-status reply))
         (text (request-text reply)))
    (dp (print-curl 'post uri headers data))
    (if (success? status)
      text
      (begin
        (displayln (format "Error: Failure on a post. got ~a text: ~a~%" status text))
        #f))))

(def (do-put uri headers data)
  (dp (print-curl 'put uri headers data))
  (let* ((reply (http-put uri
                          headers: headers
                          data: data)))
    reply))

(def (remove-bad-matches vars omit)
  (let ((goodies []))
    (for (var vars)
      (unless (string-contains var omit)
        (set! goodies (cons var goodies))))
    (reverse goodies)))

(def (interpol str)
  (displayln (interpol-from-env str)))

(def (make-format-safe str)
  "Replace all ~ to ~~ except [~ to be safe for format use"
  (if (not (string? str))
    str
    (let ((regy (pregexp "(?:[^\\[])(\\~+)")))
      (pregexp-replace* regy str " ∼"))))

(def (hash-interpol re delim str hsh fmt)
  "Given a RE, replace all instances in str with val from key matching RE"
  ;;(displayln "re: " re " delim: " delim " str: " str " fmt: " fmt)
  (if (not (and (string? str)
                (hash-table? hsh)
                re))
    str
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
    (try
     (apply format newstr (reverse set-vars))
     (catch (e)
       str)))))

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
      (apply format newstr (reverse set-vars)))))

(def (match-regexp pat str . opt-args)
  "Like pregexp-match but for all matches til end of str.
   Returns a list of captured group strings (or full match when no groups)."
  (let ((n (string-length str))
        (results []))
    (if (= n 0)
      []
      (let lp ((start 0))
        (let* ((pp (pregexp-match-positions pat str start n))
               (ix-pr (pregexp-match pat str start n)))
          (if ix-pr
            (let ((pos (+ 1 (cdar pp)))
                  (capture (if (> (length ix-pr) 1)
                             (cdr ix-pr)
                             ix-pr)))
              (set! results (append results capture))
              (if (< pos n)
                (lp pos)
                results))
            results))))))

(def (style-output infos (style "org-mode"))
  (unless JSON
    (when (list? infos)
      (let* ((sizes (hash))
             (data (reverse infos))
             (header (car data))
             (rows (cdr data))
             (sorted #f)
             (header-sep "|"))

        (for (head header)
          (unless (string? head)
            (error "style-output: header element is not a string" head))
          (hash-put! sizes head (string-length head)))
        (for (row rows)
          (let (count 0)
            (for (column row)
              (let* ((col-name (nth count header))
                     (current-size (hash-ref sizes col-name))
                     (this-size (if (string? column) (string-length column) (string-length (format "~a" column)))))
                (when (> this-size current-size)
                  (hash-put! sizes col-name this-size))
                (set! count (1+ count))))))

        (cond
         ((string=? style "org-mode")
          (set! header-sep "| ")
          (set! sorted #t))
         ((string=? style "confluence-markdown")
          (set! header-sep "||")))

        (for (head header)
          (display
           (format "~a~a" header-sep
                   (format-string-size
                    head
                    (hash-get sizes head)))))

        ;; print header
        (displayln header-sep)
        (let ((count 0))
          (for (head header)
            (let ((sep (if (= count 0) "|" "+")))
              (display
               (format "~a~a"
                       sep
                       (make-string
                        (+ 2
                           (hash-get sizes
                                     (nth count header))) #\-))))
            (set! count (1+ count))))
        (displayln "|")

        (for (row (sort rows compare-lst-car))
          (let (count 0)
            (for (col row)
              (display
               (format "| ~a"
                       (format-string-size
                        col
                        (hash-ref sizes (nth count header)))))
              (set! count (1+ count))))
          (displayln "|"))
        ))))


(def (compare-lst-car a b)
  (string<? (car a) (car b)))

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
      (display (format "| ~a " datum)))
    (displayln "|")))

(def (date->custom dt)
  (date->string (string->date dt "~Y-~m-~dT~H:~M:~S~z") "~a ~b ~d ~Y"))

(def (resolve-ipv4 host)
  (let* ((host-info (host-info-addresses (host-info host))))
    (dp (format "host-info: ~a type:~a" host-info (##type-id host-info)))
    (ip4-address->string
     (car host-info))))

(def (make-basic-auth user password)
  (format "Basic ~a"
          (base64-encode
           (string->utf8 (format "~a:~a" user password)))))

(def (any->int num)
  (cond
   ((number? num)
    num)
   ((string? num)
    (string->number num))
   ((void? num)
    0)
   ((eof-object? num)
    0)
   ((boolean? num)
    (if num
      1
      0))
   (else 0)))

(def (sis item)
  (if item
    item
    "N/A"))

(def (filter-headers headers fields)
  (filter (lambda (header) (member header fields)) headers))

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

  (let ((uri-encoding (make-uri-encoding-table uri-unreserved-chars)))
    (call-with-output-string
      (lambda (port)
        (parameterize ((current-output-port port))
          (write-uri-encoded str uri-encoding))))))

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
   ((hash-table? item)
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
  (when (and (my-ignore-errors (path-is-directory? path follow-symlinks?))
	         (recurse? path))
    (for-each!
     (directory-files path)
     (λ (name) (walk-filesystem-tree!
		        (path-expand name path) visit
		        recurse?: recurse? follow-symlinks?: follow-symlinks?)))))

(defalias λ lambda)

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

(def (cache-or-run cache-file expiration thunk)
  "Given a thunk (zero-argument procedure), check to see if cache exists within expiration time
   Return cached info if under expiration time.
   Otherwise, execute thunk and write to cache file.
   Returning data.
   Note: thunk should be a procedure, not an expression to eval.

   SECURITY NOTE: Uses read-obj-from-file/write-obj-to-file which have known
   security vulnerabilities. Consider using cache-or-run-json for new code."
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
        ;; Call the thunk directly instead of using eval
        (set! results (thunk))
        (write-obj-to-file cache-file results)))
    results))

(def (cache-or-run-json cache-file expiration thunk)
  "Safe JSON-based caching function (recommended over cache-or-run).
   Given a thunk (zero-argument procedure), check if cache exists within expiration time.
   Return cached info if under expiration time.
   Otherwise, execute thunk and write to cache file as JSON.
   Returning data.
   Note: thunk should be a procedure, not an expression to eval.
   Note: Cached data must be JSON-serializable (hash tables, lists, strings, numbers, booleans)."
  (dp (present-item cache-file))
  (let* ((results #f)
         (cfe (file-exists? cache-file))
         (ms (when cfe (modified-since? cache-file expiration))))
    (if (and cfe ms)
      (begin
        (dp "cache-or-run-json: cache hit!")
        (set! results (read-json-from-file cache-file)))
      (begin
        (dp "cache-or-run-json: cache miss :[")
        (set! results (thunk))
        (write-json-to-file cache-file results)))
    results))

(def (write-obj-to-file out-file obj)
  "DEPRECATED: Serialize object using Gambit's object serialization.

   CRITICAL SECURITY WARNING - REMOTE CODE EXECUTION RISK:
   The corresponding read-obj-from-file uses u8vector->object which can
   execute ARBITRARY CODE during deserialization. If the file is tampered
   with or comes from an untrusted source, an attacker can run any code.

   DO NOT use for: files from internet, user-modifiable files, shared locations.
   USE write-json-to-file INSTEAD for all new code.

   This function is kept only for backward compatibility with existing cache files."
  (with-output-to-file [ path: out-file create: 'maybe truncate: #t ]
    (lambda (out)
      (write-string (base64-encode (object->u8vector obj)) out))))

(def (write-string-to-file out-file str)
  "Write the contents of str to out-file"
  (with-output-to-file [ path: out-file create: 'maybe truncate: #t ]
    (lambda (out)
      (write-string str out))))

(def (write-json-to-file out-file obj)
  "Serialize object to a file as JSON. Safe for untrusted data."
  (with-output-to-file [ path: out-file create: 'maybe truncate: #t ]
    (lambda (out)
      (write-string (json-object->string obj) out))))

(def (read-obj-from-file in-file)
  "DEPRECATED: Deserialize object using Gambit's object serialization.

   CRITICAL SECURITY WARNING - REMOTE CODE EXECUTION RISK:
   This function uses u8vector->object which can execute ARBITRARY CODE
   during deserialization. If the input file has been tampered with or
   comes from an untrusted source, an attacker can execute any code.

   DO NOT use for: files from internet, user-modifiable files, shared locations.
   USE read-json-from-file INSTEAD for all new code.

   This function is kept only for backward compatibility with existing cache files."
  (try
   (u8vector->object
    (base64-decode
     (read-file-string in-file)))
   (catch (e)
     (display-exception e)
     #f)))

(def (read-json-from-file in-file)
  "Deserialize JSON object from a file. Safe for untrusted data."
  (try
   (parameterize ((read-json-key-as-symbol? #t))
     (with-input-from-string (read-file-string in-file) read-json))
   (catch (e)
     (display-exception e)
     #f)))

(def (modified-since? file secs-ago)
  "Check file mtime and determine if file is older than secs-ago"
  (if (file-exists? file)
    (let* ((now (float->int (time->seconds (current-time))))
           (mtime (time->seconds (file-info-last-modification-time (file-info file))))
           (diff (- now mtime)))
      (if (< diff secs-ago)
        #t
        #f))
    #f))

(defrules my-ignore-errors ()
  ((_ form ...) (with-catch (λ (_) #f) (λ () form ...))))

(def (rekey-sym hsh)
  "Convert all keys from strings to symbols, nondestructively"
  (unless (hash-table? hsh)
    (error "hsh is not table." (##type-id hsh)))
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

(def (marshal-value value)
  (let* ((buf (open-buffered-writer #f))
         (w (BufferedWriter-marshal buf value)))
    (get-buffer-output-u8vector buf)))

(def (unmarshal-value pickle)
  (let ((buf (open-buffered-reader pickle)))
    (BufferedReader-unmarshal buf)))

(def (my-json-object->string obj)
  "Handle the change to master of using strings as default keys"
  (parameterize ((read-json-key-as-symbol? #t))
    (json-object->string obj)))

(def (mixed-string-join lst sep)
  (string-join
   (map
     (lambda (x)
       (cond
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        (else x)))
     lst)
   sep))

(def (def-num num)
  (if (string? num)
    (string->number num)
    num))

;; (def (generate-http-functions openapi-json)
;;   (let* ((spec (json->object openapi-json))
;;          (paths (hash-ref spec "paths")))
;;     (for-each (lambda (path-entry)
;;                 (let* ((path (car path-entry))
;;                        (ops (cdr path-entry))
;;                        (ops-list (hash->list ops)))
;;                   (for-each (lambda (op)
;;                               (let* ((method (string->symbol (car op)))
;;                                      (op-spec (cdr op))
;;                                      (summary (hash-ref op-spec "summary"))
;;                                      (parameters (hash-ref op-spec "parameters"))
;;                                      (request-body (hash-ref op-spec "requestBody"))
;;                                      (responses (hash-ref op-spec "responses")))
;;                                 (printf "~a ~a~%" method summary)
;;                                 (printf "(define (~a url~@
;;                                                   ~a~@
;;                                                   ~a)~%"
;;                                         (string->symbol
;;                                          (string-append
;;                                           (symbol->string method)
;;                                           "-"
;;                                           (string-replace path "/" "-")))
;;                                         (if (null? parameters)
;;                                           ""
;;                                           "#!optional")
;;                                         (if (null? request-body)
;;                                           ""
;;                                           "#!optional (body #f)"))
;;                                 (printf "  (http-~a url~%" (symbol->string method))
;;                                 (when (not (null? parameters))
;;                                   (printf "   (params~%")
;;                                   (for-each (lambda (param)
;;                                               (printf "     ~s~%" (hash-ref param "name")))
;;                                             parameters)
;;                                   (printf "   )"))
;;                                 (when (not (null? request-body))
;;                                   (printf "   (body: body)"))
;;                                 (printf "))~%~%")))
;;                             ops-list)))
;;               (hash->list paths))))
