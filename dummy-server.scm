(define-module dummy-server
  (use gauche.net)
  (use rfc.822)
  (use srfi-13)
  (use util.match)
  (use util.list)
  (use rfc.uri)
  (use file.util)
  (use gauche.uvector)
  (export run-dummy-server))

(select-module dummy-server)

(debug-print-width 4096)

(define *s3-endpoint* "s3.amazonaws.com")

(define (run-dummy-server port mode)
  (let1 pid (sys-fork)
    (if (= pid 0)
        (cond ((equal? mode 'record) (s3-proxy port))
              ((equal? mode 'replay) (s3-server port)))
        (begin (sys-sleep 2) pid))))

(define (s3-server port)
  (let1 dat (file->sexp-list "dummy-server.dat")
    (let1 ssock (make-server-socket 'inet port :reuse-addr? #t)
      (let loop ((dat dat))
        (let* ((csock (socket-accept ssock))
               (cin   (socket-input-port csock))
               (cout  (socket-output-port csock)))
          (receive (stat-line hdrs maybe-body) (http-receive cin)
            ;; return data
            (match-let1 (stat-line hdrs maybe-body) (car dat)
              (http-send cout stat-line hdrs maybe-body))
            (close-output-port cout)
            (socket-close csock)
            (loop (cdr dat))))))))

(define (s3-proxy port)
  (when (file-exists? "dummy-server.dat")
    (sys-unlink "dummy-server.dat"))
  (let1 ssock (make-server-socket 'inet port :reuse-addr? #t)
    (display "server start.\n")
    (let loop ()
      (let* ((csock (socket-accept ssock))
             (cin   (socket-input-port csock))
             (cout  (socket-output-port csock)))
        (receive (stat-line hdrs maybe-body) (http-receive cin)
          (receive (stat-line hdrs maybe-body)
              (http-req *s3-endpoint* stat-line hdrs maybe-body)
            ;; send data to client
            (call-with-output-file "dummy-server.dat"
              (lambda (port)
                (format port "~s\n" (list stat-line hdrs maybe-body)))
              :if-exists :append)
            (http-send cout stat-line hdrs maybe-body)))
        (close-output-port cout)
        (socket-close csock))
      (loop))))

(define (http-req endpoint stat-line hdrs maybe-body)
  (match-let1 (method path ver) (string-tokenize stat-line)
    (call-with-client-socket (make-client-socket endpoint 80)
      (lambda (in out)
        (format out "~a ~a HTTP/1.0\r\n" method path)
        (format out "~a\r\n\r\n"
                (string-join (map (cut string-join <> ": ") hdrs) "\r\n"))
        (when maybe-body
          (let ((clen (rfc822-header-ref hdrs "content-length"))
                (tenc (rfc822-header-ref hdrs "transfer-encoding")))
            (cond [clen (display maybe-body out)]
                  [tenc (put-chunked out maybe-body)]))
          (flush out))
        (receive (stat-line2 hdrs2 maybe-body2)
            (http-receive in :head? (equal? method "HEAD"))
          (match-let1 (ver code desc ...) (string-tokenize stat-line2)
            (if (#/^3/ code)
                (receive (scheme user host port path query frag)
                    (uri-parse (rfc822-header-ref hdrs2 "location"))
                  (http-req host stat-line hdrs maybe-body))
                (values stat-line2 hdrs2 maybe-body2))))
        ))))

(define (http-receive iport :key (head? #f))
  (define (get-method line)
    (string-scan line " " 'before))
  (let* ((stat-line (read-line iport))
         (hdrs      (rfc822-read-headers iport)))
    (let1 maybe-body (let ((clen (rfc822-header-ref hdrs "content-length"))
                           (tenc (rfc822-header-ref hdrs "transfer-encoding")))
                       (cond [head? #f]
                             [(and clen
                                   (not (equal? (get-method stat-line) "HEAD")))
                              (read-block (x->number clen) iport)]
                             [(equal? tenc "chunked") (get-chunked iport)]
                             [(#/^HTTP/ stat-line) (port->string iport)]
                             [else #f]))
      (let1 maybe-body (if (eof-object? maybe-body) #f maybe-body)
        (values stat-line hdrs maybe-body)))))

(define (http-send oport stat-line hdrs maybe-body)
  (format oport #`",|stat-line|\r\n")
  (format oport "~a\r\n\r\n"
          (string-join (map (cut string-join <> ": ") hdrs) "\r\n"))
  (when maybe-body
    (let ((clen (rfc822-header-ref hdrs "content-length"))
          (tenc (rfc822-header-ref hdrs "transfer-encoding")))
      (cond [clen (display maybe-body oport)]
            [tenc (put-chunked oport maybe-body)]
            [maybe-body (display maybe-body oport)]))
    (flush oport)))

(define (get-chunked iport)
  (let loop ((buf '()))
    (let1 size (x->number #`"#x,(read-line iport)")
      (if (equal? size 0)
          (begin (read-line iport)
                 (read-line iport)
                 (string-join (reverse buf) ""))
          (loop (cons (read-block size iport) buf))))))

(define (put-chunked oport body)
  (format oport "~x\r\n" (string-length body))
  (format oport body)
  (format oport "\r\n0\r\n\r\n"))

  (define (read-block len iport)
    (let1 vec (make-u8vector len 0)
      (let loop ((start 0) (remain len))
        (if (> len 0)
            (let1 len (read-block! vec iport start)
              (if (eof-object? len)
                  (u8vector->string vec)
                  (loop (+ start len) (- remain len))))
            (u8vector->string vec)))))

(define (assoc-get alist key)
  (cond ((assoc key alist) => cadr)
        (else #f)))

(provide "dummy-server")