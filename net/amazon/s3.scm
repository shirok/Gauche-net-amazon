;;;
;;; Amazon Web Services - Simple Storage Service
;;;

(define-module net.amazon.s3
  (use gauche.sequence)
  (use gauche.experimental.ref)
  (use gauche.experimental.app)
  (use gauche.experimental.lamb)
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use rfc.822)
  (use rfc.uri)
  (use rfc.sha)
  (use rfc.hmac)
  (use rfc.base64)
  (use rfc.http)
  (use util.list)
  (use text.tree)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use sxml.tools)
  (extend net.amazon.base)
  (export s3-bucket-list     s3-bucket-list/raw
          s3-bucket-availability
          s3-bucket-create!  s3-bucket-create/raw!
          s3-bucket-delete!  s3-bucket-delete/raw!
          s3-bucket-location s3-bucket-location/raw
          s3-object-list     s3-object-list/raw
          s3-object-get      s3-object-get/raw
          s3-object-head     s3-object-head/raw
          s3-object-put!     s3-object-put/raw!
          s3-object-copy!    s3-object-copy/raw!
          s3-object-delete!  s3-object-delete/raw!
          s3-auth-header-value
          s3-signature s3-sign-string s3-string-to-sign))
(select-module net.amazon.s3)

(define *s3-endpoint* "s3.amazonaws.com")

;; In general, two procedures are provided for each S3 API.
;; A procedure with /raw suffix returns two values: the SXML of
;; Amazon's response, and the list of headers (in the form that
;; rfc822-header-ref can be used).  You can get complete information
;; from those values.
;; A procedure without /raw suffix is a convenience procedure;
;; it extracts and returns the value that are handy for typical
;; applications.  For example, s3-bucket-list returns a list of
;; (<bucket-name> <creation-date>), where <cretion-date> is Gauche's
;; <date> object.
;; We hope the convenience interface is sufficient for most tasks,
;; though it does drop auxiliary information, and you may need to
;; use /raw interface time to time.

;;-------------------------------------------------------------------
;; Operations
;;
;; AWS API spec: http://docs.amazonwebservices.com/AmazonS3/2006-03-01/API/index.html

;; Buckets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns ((Name Date) ...)
(define (s3-bucket-list)
  (receive (sxml _) (s3-bucket-list/raw)
    (map (^s (list ((if-car-sxpath '(// aws:Name *text*)) (list s))
                   (iso-8601-date->date
                    ((if-car-sxpath '(// aws:CreationDate *text*)) (list s)))))
         ((sxpath '(// aws:Bucket)) sxml))))

;; Returns Sxml and Headers
(define (s3-bucket-list/raw)
  (values-ref (s3-request-response 'GET #f "/" '()) 2 1))

;; Returns either 'used, 'available or 'taken.
(define (s3-bucket-availability bucket)
  (receive (status hdrs response)
      (s3-request-response 'HEAD bucket "/" '(("max-keys" 0)))
    (cond
     [(equal? status "200") 'used]
     [(equal? status "404") 'available]
     [(equal? status "403") 'taken]
     [else (errorf <aws-server-error> :status status :headers hdrs :body '()
                   "Access failed (~a)" status)])))

(define (s3-bucket-create! bucket :key (location #f))
  (s3-bucket-create/raw! bucket :location location)
  (values))

(define (s3-bucket-create/raw! bucket :key (location #f))
  (define body (if location
                 `(CreateBucketConfiguration (LocationConstraint ,location))
                 ""))
  (values-ref (s3-request-response 'PUT bucket "/" '() body) 2 1))

(define (s3-bucket-delete! bucket)
  (s3-bucket-delete/raw! bucket)
  (values))

(define (s3-bucket-delete/raw! bucket)
  (values-ref (s3-request-response 'DELETE bucket "/" '()) 2 1))

;; Returns either 'EU, 'us-west-1 or 'us-classic.
(define (s3-bucket-location bucket)
  (string->symbol (or ((if-car-sxpath '(// aws:LocationConstraint *text*))
                       (values-ref (s3-bucket-location/raw bucket) 0))
                      "us-classic")))

(define (s3-bucket-location/raw bucket)
  (values-ref (s3-request-response 'GET bucket "/?location" '()) 2 1))

;; Objects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (s3-object-list bucket :key (prefix #f) (marker #f)
                        (max-keys #f) (delimiter #f))
  (receive (sxml _) (s3-object-list/raw bucket
                                         :prefix prefix :marker marker
                                         :max-keys max-keys
                                         :delimiter delimiter)
    ((sxpath '(// aws:Key *text*)) sxml)))

(define (s3-object-list/raw bucket :key (prefix #f) (marker #f)
                                               (max-keys #f) (delimiter #f))
  (let1 q (http-compose-query "/"
                              (cond-list [prefix @ `((prefix ,prefix))]
                                         [marker @ `((marker ,marker))]
                                         [max-keys @ `((max-keys ,max-keys))]
                                         [delimiter @ `((delimiter ,delimiter))]))
    (values-ref (s3-request-response 'GET bucket q '()) 2 1)))

(define (s3-object-put! bucket id data :key (cache-control #f)
                        (content-disposition #f) (content-encoding #f)
                        (content-length #f) (content-md5 #f)
                        (content-type #f) (expect #f) (expires #f)
                        (acl #f) (meta #f))
  (s3-object-put/raw! bucket id data :cache-control cache-control
                      :content-disposition content-disposition
                      :content-encoding content-encoding
                      :content-length content-length
                      :content-md5 content-md5 :content-type content-type
                      :expect expect :expires expires :acl acl :meta meta)
  (values))

(define (s3-object-put/raw! bucket id data :key (cache-control #f)
                            (content-disposition #f) (content-encoding #f)
                            (content-length #f) (content-md5 #f)
                            (content-type #f) (expect #f) (expires #f)
                            (acl #f) (meta #f))
  (values-ref (s3-request-response 'PUT bucket #`"/,|id|"
                                   (append
                                    (cond-list [cache-control @ `((cache-control ,cache-control))]
                                               [content-disposition @ `((content-disposition ,content-disposition))]
                                               [content-encoding @ `((content-encoding ,content-encoding))]
                                               [content-length @ `((content-length ,content-length))]
                                               [content-md5 @ `((content-md5 ,content-md5))]
                                               [content-type @ `((content-type ,content-type))]
                                               [expect @ `((expect ,expect))]
                                               [expires @ `((expires ,expires))]
                                               [expires @ `((expires ,expires))]
                                               [acl @ `((x-amz-acl ,acl))])
                                    (if meta
                                        (map (lambda (rec)
                                               `((,(string->symbol #`"x-amz-meta-,(car rec)") ,(cadr rec))))
                                             meta)
                                        '()))
                                   data)
              2 1))

(define (s3-object-get bucket id :key (range #f) (if-modified-since #f)
                       (if-unmodified-since #f) (if-match #f)
                       (if-none-match #f))
  (receive (text _) (s3-object-get/raw bucket id :range range
                                       :if-modified-since if-modified-since
                                       :if-unmodified-since if-unmodified-since
                                       :if-match if-match
                                       :if-none-match if-none-match)
    text))

(define (s3-object-get/raw bucket id :key (range #f) (if-modified-since #f)
                           (if-unmodified-since #f) (if-match #f)
                           (if-none-match #f))
  (values-ref (s3-request-response 'GET bucket #`"/,|id|"
                                   (cond-list [range @ `((range ,range))]
                                              [if-modified-since @ `((if-modified-since ,if-modified-since))]
                                              [if-unmodified-since @ `((if-unmodified-since ,if-unmodified-since))]
                                              [if-match @ `((if-match ,if-match))]
                                              [if-none-match @ `((if-none-match ,if-none-match))]
                                              ))
              2 1))

(define (s3-object-head bucket id  :key (range #f) (if-modified-since #f)
                        (if-unmodified-since #f) (if-match #f)
                        (if-none-match #f))
  (receive (_ hdrs) (s3-object-head/raw bucket id :range range
                                        :if-modified-since if-modified-since
                                        :if-unmodified-since if-unmodified-since
                                        :if-match if-match
                                        :if-none-match if-none-match)
    hdrs))

(define (s3-object-head/raw bucket id  :key (range #f) (if-modified-since #f)
                            (if-unmodified-since #f) (if-match #f)
                            (if-none-match #f))
  (values-ref (s3-request-response 'HEAD bucket #`"/,|id|"
                                   (cond-list [range @ `((range ,range))]
                                              [if-modified-since @ `((if-modified-since ,if-modified-since))]
                                              [if-unmodified-since @ `((if-unmodified-since ,if-unmodified-since))]
                                              [if-match @ `((if-match ,if-match))]
                                              [if-none-match @ `((if-none-match ,if-none-match))]
                                              ))
              2 1))

(define (s3-object-copy! bucket src dstid :key (acl #f) (metadata-directive #f)
                         (copy-source-if-match #f)
                         (copy-source-if-none-match #f)
                         (copy-source-if-unmodified-since #f)
                         (copy-source-if-modified-since #f))
  ((if-car-sxpath '(// CopyObjectResult ETag *text*))
   (s3-object-copy/raw! bucket src dstid)))

(define (s3-object-copy/raw! bucket src dstid :key (acl #f)
                             (metadata-directive #f)
                             (copy-source-if-match #f)
                             (copy-source-if-none-match #f)
                             (copy-source-if-unmodified-since #f)
                             (copy-source-if-modified-since #f))
  (values-ref (s3-request-response 'PUT bucket #`"/,|dstid|"
                                   (append `(("x-amz-copy-source" ,src))
                                           (cond-list [acl @ `((x-amz-acl ,acl))]
                                                      [metadata-directive @ `((x-amz-metadata-directive ,metadata-directive))]
                                                      [copy-source-if-match @ `((x-amz-copy-source-if-match ,copy-source-if-match))]
                                                      [copy-source-if-none-match @ `((x-amz-copy-source-if-none-match ,copy-source-if-none-match))]
                                                      [copy-source-if-unmodified-since @ `((x-amz-copy-source-if-unmodified-since ,copy-source-if-unmodified-since))]
                                                      [copy-source-if-modified-since @ `((x-amz-copy-source-if-modified-since ,copy-source-if-modified-since))]))
                                   "")
              2 1))

(define (s3-object-delete! bucket id :key (mfa #f))
  (receive (sxml _) (s3-object-delete/raw! bucket id :mfa mfa)
    sxml))

(define (s3-object-delete/raw! bucket id :key (mfa #f))
  (values-ref (s3-request-response 'DELETE bucket #`"/,|id|"
                                   (cond-list [mfa @ `(x-amz-mfa ,mfa)]))
              2 1))

(debug-print-width 1024)

;;-------------------------------------------------------------------
;; Common Request-response handling
;;

;; Returns status, headers, and body in sxml.
;; Most errors are handled, except HEAD request, in which non-200 status
;; are returned as is.
(define (s3-request-response method bucket uri headers :optional (body #f))
  (define reqproc
    (or (assoc-ref `((GET . ,http-get)
                     (HEAD . ,http-head)
                     (POST . ,http-post)
                     (PUT . ,http-put)
                     (DELETE . ,http-delete))
                   method)
        (error <aws-invalid-parameter> "Bad request method:" method)))
  (define cbody                         ;canonicalized body
    (cond [(not body) #f]
          [(string? body) body]
          [(pair? body) ($ tree->string $ sxml:sxml->xml body)]))
    
  (receive (status hdrs body)
      (safe-http-req reqproc uri cbody
                     (prepare-http-headers method bucket uri headers cbody))
    (case method
      [(HEAD) (values status hdrs #f)]
      [else
       (if (#/^20.$/ status)
           (if (member (assoc-ref hdrs "content-type")
                       '(("text/plain") ("binary/octet-stream")))
               (values status hdrs body)
               (let1 sxml (safe-parse-xml body status hdrs)
                 (values status hdrs sxml)))
           (let1 sxml (safe-parse-xml body status hdrs)
             (server-error status hdrs sxml)))])))

(define (prepare-http-headers method bucket uri headers body)
  (define (ensure-date hdrs)
    (cond [(rfc822-header-ref headers "date") hdrs]
          [else `(("date" ,(date->rfc822-date (current-date))) ,@hdrs)]))
  (define (ensure-host hdrs)
    (cond [(rfc822-header-ref headers "host") hdrs]
          [bucket `(("host" ,#`",|bucket|.,*s3-endpoint*") ,@hdrs)]
          [else   `(("host" ,*s3-endpoint*) ,@hdrs)]))
  (define (ensure-content-length hdrs)
    (cond [(not body) hdrs]
          [else `(("content-length" ,(x->string (string-size body))) ,@hdrs)]))

  (let1 h ($ ensure-content-length $ ensure-host $ ensure-date headers)
    `(,@(append-map (^p `(,(make-keyword (car p)) ,(cadr p))) h)
      :authorization ,(s3-auth-header-value
                       (s3-signature (x->string method) h bucket uri)))))

(define (safe-http-req proc path maybe-body http-headers)
  (guard (e [else (error <aws-net-error> :cause e :message (~ e'message))])
    (apply proc *s3-endpoint* path
           (if maybe-body (cons maybe-body http-headers) http-headers))))

(define (safe-parse-xml body status replyhdrs)
  (define ns '((aws . "http://s3.amazonaws.com/doc/2006-03-01/")))
  (if (or (not body)
          (equal? (rfc822-header-ref replyhdrs "content-length") "0"))
    '()                                 ;empty body
    (guard (e [else (error <aws-parse-error> :cause e :message (~ e'message)
                           :status status :headers replyhdrs :body body)])
      (call-with-input-string body (cut ssax:xml->sxml <> ns)))))

(define (server-error status hdrs sxml)
  (errorf <aws-server-error> :status status :headers hdrs :body sxml
          "~a (~a)"
          ((if-car-sxpath '(// Error Message *text*)) sxml)
          ((if-car-sxpath '(// Error Code *text*)) sxml)))

;;-------------------------------------------------------------------
;; Signing
;;

(define (s3-auth-header-value signature :key (access-key (aws-access-key-id)))
  (ensure-access-key access-key)
  #`"AWS ,|access-key|:,|signature|")

;; http://docs.amazonwebservices.com/AmazonS3/2006-03-01/index.html?RESTAuthentication.html
(define (s3-signature  http-verb    ; <string>
                           headers      ; ((<string> <string>) ...)
                           bucket       ; <string>?
                           resource-uri ; <string>
                           :key (secret (aws-secret-access-key))
                                (expires #f))
  (ensure-secret secret)
  (s3-sign-string
   (s3-string-to-sign http-verb headers bucket resource-uri expires)
   :secret secret))

(define (s3-sign-string string :key (secret (aws-secret-access-key)))
  ($ base64-encode-string
     $ hmac-digest-string string :key secret :hasher <sha1>))

(define (s3-string-to-sign http-verb    ; <string>
                           headers      ; ((<string> <string>) ...)
                           bucket       ; <string>?
                           resource-uri ; <string>
                           expires)     ; <number>?
  (let1 chdrs (map (^p (cons (string-downcase (car p)) (cdr p))) headers)
    (string-append http-verb "\n"
                   (rfc822-header-ref chdrs "content-md5" "") "\n"
                   (rfc822-header-ref chdrs "content-type" "") "\n"
                   (cond [expires (x->string expires)]
                         [(rfc822-header-ref chdrs "x-amz-date") ""]
                         [else (rfc822-header-ref chdrs "date" "")])
                   "\n"
                   (canonicalize-amz-headers chdrs)
                   (canonicalize-resource bucket resource-uri))))

(define (canonicalize-amz-headers headers)
  ($ string-concatenate
     $ map (^p (string-append (caar p)":"(string-join (map cadr p) ",")"\n"))
     $ group-sequence (sort-by (filter (^x (#/^x-amz-/ (car x))) headers) car)
       :key car :test string=?))

(define (canonicalize-resource bucket resource-uri)
  (receive (a path q f) (uri-decompose-hierarchical resource-uri)
    (string-append (if bucket #`"/,bucket" "") path
                   (if (member q '("location" "acl" "torrent")) #`"?,q" ""))))

;;-------------------------------------------------------------------
;; Utilities
;;

(define (ensure-secret secret)
  (unless secret
    (error <aws-invalid-parameter> "Secret Access Key isn't set.")))
(define (ensure-access-key access-key)
  (unless access-key
    (error <aws-invalid-parameter> "Access Key ID isn't set.")))

(define iso-8601-rx
  #/^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})(?:\.\d+)?(?:(?:([+-])(\d{2}):?(\d{2})?)?Z)?$/)

(define (iso-8601-date->date sdate)
  (and-let* ([ sdate ]
             [m (iso-8601-rx sdate)])
    (make-date 0 (x->integer (m 6)) (x->integer (m 5)) (x->integer (m 4))
               (x->integer (m 3)) (x->integer (m 2)) (x->integer (m 1))
               (* (if (equal? (m 7) "+") +1 -1)
                  (+ (* (x->integer (m 8)) 3600)
                     (* (x->integer (m 9)) 60))))))

(define (sxml->string sxml) (tree->string (sxml:sxml->xml sxml)))

;; TODO: This will be in rfc.822 after 0.9.1.  Don't forget to remove.
(define (date->rfc822-date date)
  (let1 tz (date-zone-offset date)
    (format "~a, ~2d ~a ~4d ~2,'0d:~2,'0d:~2,'0d ~a~2,'0d~2,'0d"
            (ref '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
                 (date-week-day date))
            (date-day date)
            (ref '#("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                 (date-month date))
            (date-year date)
            (date-hour date) (date-minute date) (date-second date)
            (if (>= tz 0) "+" "-")
            (quotient (abs tz) 3600)
            (modulo (quotient (abs tz) 60) 60))))

;; patching rfc.http as of 0.9.   remove this after 0.9.1 release.
(with-module rfc.http
(define (request-response request conn host request-uri request-body options)
  (define no-body-replies '("204" "304"))
  (receive (host uri)
      (consider-proxy conn (or host (ref conn'server)) request-uri)
    (with-connection
     conn
     (lambda (in out)
       (send-request out request host uri request-body options)
       (receive (code headers) (receive-header in)
         (values code
                 headers
                 (and (not (eq? request 'HEAD))
                      (not (member code no-body-replies))
                      (let-keywords options
                          ((sink    (open-output-string))
                           (flusher (lambda (sink _) (get-output-string sink)))
                           . #f)
                        (receive-body in headers sink flusher)))))))))
)
