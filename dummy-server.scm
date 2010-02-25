(define-module dummy-server
  (use gauche.net)
  (use rfc.822)
  (use srfi-13)
  (use util.match)
  (use util.list)
  (export run-dummy-server))

(select-module dummy-server)

(debug-print-width 4096)

(define-constant *mode* 'proxy)
(define *s3-endpoint* "s3.amazonaws.com")

(define (run-dummy-server port)
  (let1 pid (sys-fork)
    (if (= pid 0)
        (if (equal? *mode* 'proxy)
            (s3-proxy port)
            (s3-server port))
        pid)))

(define (s3-server port)
  (let1 ssock (make-server-socket 'inet port :reuse-addr? #t)
    (let loop ()
      (let* ((csock (socket-accept ssock))
             (cin   (socket-input-port csock))
             (cout  (socket-output-port csock)))
        (receive (stat-line hdrs maybe-body) (http-receive cin)
          ;; return data
          )))))

(define (s3-proxy port)
  (let1 ssock (make-server-socket 'inet port :reuse-addr? #t)
    (let loop ()
      (let* ((csock (socket-accept ssock))
             (cin   (socket-input-port csock))
             (cout  (socket-output-port csock)))
        (receive (stat-line hdrs maybe-body) (http-receive cin)
          (call-with-client-socket (make-client-socket 'inet *s3-endpoint* 80)
            (lambda (in out)
              (http-send out
                         stat-line
                         (assoc-set! hdrs "host" `(,*s3-endpoint*))
                         maybe-body)
              (receive (stat-line hdrs maybe-body) (http-receive in)
                (http-send cout stat-line hdrs maybe-body))))
          (close-output-port cout)
          (socket-close csock)))
      (loop))))

(define (http-receive iport)
  (let* ((stat-line (read-line iport))
         (hdrs      (rfc822-read-headers iport)))
    (match-let1 (method path ver) (string-tokenize stat-line)
      (let1 maybe-body (let ((clen (assoc-get hdrs "content-length"))
                             (tenc (assoc-get hdrs "transfer-encoding")))
                         (cond [clen (read-block (x->number clen) iport)]
                               [(equal? tenc "chunked") (get-chunked iport)]
                               [else #f]))
        (values stat-line hdrs maybe-body)))))

(define (http-send oport stat-line hdrs maybe-body)
  (format oport #`",|stat-line|\r\n")
  (format oport "~a\r\n\r\n"
          (string-join (map (cut string-join <> ": ") hdrs) "\r\n"))
  (when maybe-body (put-chunked oport maybe-body)))

(define (get-chunked iport)
  (let loop ((buf '()))
    (let1 size (x->number #`"#x,(read-line iport)")
      (if (equal? size 0)
          (string-join (reverse buf))
          (loop (cons (read-block size iport) buf))))))

(define (put-chunked oport body)
  (format oport "~x\r\n" (string-length body))
  (format oport body)
  (format oport "\r\n0\r\n\r\n"))

(define (assoc-get alist key)
  (cond ((assoc key alist) => cadr)
        (else #f)))

(provide "dummy-server")