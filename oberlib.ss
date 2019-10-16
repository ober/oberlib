;;; -*- Gerbil -*-
;;; © ober
;;; my utils

(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :scheme/base
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/db/dbi
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


(def (format-string-size string size)
  (unless (string? string)
    (set! string (format "~a" string)))
  (let* ((string (string-trim-both string))
         (our-size (string-length string))
         (delta (if (> size our-size)
                  (- size our-size)
                  0)))
    ;;    (displayln "fss: delta: " delta " string: " string " our-size: " our-size " size: " size)
    (format " ~a~a" string (make-string delta #\space))))

(def (style-output infos)
  (when (list? infos)
    (let* ((sizes (hash))
           (data (reverse infos))
           (header (car data))
           (rows (cdr data)))
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

      (for (head header)
           (display (format "| ~a" (format-string-size head (hash-get sizes head)))))

      ;; print header
      (displayln "|")
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
   (else
    (displayln "Unknown format: " style))))

(def (print-row style data)
  (if (list? data)
    (cond
     ((string=? style "org-mode")
      (org-mode-print-row data))
     (else
      (displayln "Unknown format! " style)))))

(def (org-mode-print-row data)
  (when (list? data)
    (for (datum data)
         (printf "| ~a " datum))
    (displayln "|")))

(def (resolve-ipv4 host)
  (let* ((host-info (host-info-addresses (host-info host))))
    (dp (format "host-info: ~a type:~a" host-info (type-of host-info)))
    (ip4-address->string
     (car host-info))))

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

(def (make-basic-auth user password)
  (format "Basic ~a"
	  (base64-encode
	   (string->utf8 (format "~a:~a" user password)))))

(def (success? status)
  (and (>= status 200) (<= status 299)))

(def (do-post uri headers data)
  (dp (print-curl "post" uri headers data))
  (try
   (let* ((reply (http-post uri
			    headers: headers
			    data: data))
	  (status (request-status reply))
	  (text (request-text reply)))

     (if (success? status)
       (displayln status text)
       (displayln (format "Failure on post. Status:~a Text:~a~%" status text))))
   (catch (e)
     (begin
       (display-exception e)))))

(def (do-put uri headers data)
  (dp (print-curl "put" uri headers data))
  (let* ((reply (http-put uri
			  headers: headers
			  data: data))
	 (status (request-status reply))
	 (text (request-text reply)))

    (if (success? status)
      (displayln text)
      (displayln (format "Failure on post. Status:~a Text:~a~%" status text)))))

(def (do-delete uri headers params)
  (dp (print-curl "delete" uri headers params))
  (let* ((reply (http-delete uri
			     headers: headers
			     params: params))
	 (status (request-status reply))
	 (text (request-text reply)))

    (if (success? status)
      (displayln text)
      (displayln (format "Failure on delete. Status:~a Text:~a~%" status text)))))

(def (stringify-hash h)
  (let ((results []))
    (if (table? h)
      (begin
	(hash-for-each
	 (lambda (k v)
	   (set! results (append results (list (format " ~a->" k) (format "~a   " v)))))
	 h)
	(append-strings results))
      ;;        (pregexp-replace "\n" (append-strings results) "\t"))
      "N/A")))

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

(def (do-get-generic uri headers)
  (let* ((reply (http-get uri
			  headers: headers))
	 (status (request-status reply))
	 (text (request-text reply)))
    (print-curl "get" uri "" "")
    (if (success? status)
      text
      (displayln (format "Error: got ~a on request. text: ~a~%" status text)))))