#!/usr/bin/env gosh
(use net.amazon.s3)
(use gauche.parseopt)
(use gauche.parameter)
(use srfi-13)
(use util.match)

(define (main args)
  (if (file-exists? "keys.txt")
      (receive (keyid skey) (call-with-input-file "keys.txt" keys-file-load)
        (if (and keyid skey)
            (parameterize ((aws-access-key-id     keyid)
                           (aws-secret-access-key skey))
              (run-cli))
            (errorf "ERROR: please set access-key-id and secret-access-key in keys.txt.")))
      (errorf "ERROR: please set access-key-id and secret-access-key in keys.txt."))
  0)

(define (keys-file-load iport)
  (define (assoc-get lis key)
    (cond ((assoc key lis) => (lambda (e) (and (not (null? e)) (cadr e))))
          (else #f)))
  (let1 alist (map (lambda (line)
                     (map string-trim-both (string-split line ":")))
                   (port->list read-line iport))
    (values (assoc-get alist "access-key-id")
            (assoc-get alist "secret-access-key"))))

(define (run-cli)
  (define (usage str)
    (display #`"USAGE: ,|str|\n" (current-error-port)))
  (define (warning str)
    (display #`"ERROR: ,|str|\n" (current-error-port)))
  (with-module net.amazon.s3 (set! *s3-endpoint* "localhost:8080"))
  (let ((bucket #f))
    (define (check-bucket)
      (or bucket (begin (warning "bucket is not set.") #f)))
    (let loop ()
      (display "sSh3ll> ") (flush)
      (let1 line (read-line)
        (if (eof-object? line)
            (print "quit")
            (begin (guard (e (else (warning (ref e 'message)) (raise e)))
                          (match (string-tokenize line)
                            (("bucket" bucketname)
                             (set! bucket bucketname)
                             (print #`"set bucket: ,|bucket|"))
                            (("bucket")
                             (print #`"now bucket is set to ,|bucket|"))
                            (("bucket" . args)
                             (usage "bucket [bucketname]"))

                            (("bucket-list")
                             (print (s3-bucket-list)))

                            (("bucket-available?")
                             (when (check-bucket)
                               (print (s3-bucket-availability bucket))))

                            (("bucket-location")
                             (when (check-bucket)
                               (print (s3-bucket-location bucket))))

                            (("createbucket")
                             (when (check-bucket)
                               (s3-bucket-create! bucket)))

                            (("deletebucket")
                             (when (check-bucket)
                               (s3-bucket-delete! bucket)))

                            (("list")
                             (when (check-bucket)
                               (format #t "~s\n" (s3-object-list bucket))))

                            (("list" prefix)
                             (when (check-bucket)
                               (format #t "~s\n"
                                       (s3-object-list bucket :prefix prefix))))

                            (("list" prefix max)
                             (when (check-bucket)
                               (format #t "~s\n"
                                       (s3-object-list bucket :prefix prefix
                                                       :max-keys max))))

                            (("put" id data . rem)
                             (when (check-bucket)
                               (s3-object-put! bucket id (tokens-drop line 2))
                               (print #`"put ,|id|")))

                            (("get" id)
                             (when (check-bucket)
                               (format #t "~s\n" (s3-object-get bucket id))))
                            
                            (("head" id)
                             (when (check-bucket)
                               (format #t "~s\n" (s3-object-head bucket id))))

                            (("delete" id)
                             (when (check-bucket)
                               (format #t "~s\n" (s3-object-delete! bucket id))))

                            (("copy" src dstid)
                             (when (check-bucket)
                               (format #t "~s\n" (s3-object-copy! bucket src dstid))))

                            (("quit") (exit 0))

                            ((cmd . args)
                             (format (current-error-port)
                                     "ERROR: invalid command: ~a\n" cmd))))
                   (loop)))))))

(define (tokens-drop str num)
  (if (= num 0)
      str
      (let1 m (#/[ \t\r\n]+/ str)
        (if m
            (tokens-drop (rxmatch-after m) (- num 1))
            ""))))