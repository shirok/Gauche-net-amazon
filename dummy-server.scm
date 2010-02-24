(define-module dummy-server
  (use gauche.net)
  (use rfc.822)
  (use srfi-13)
  (use util.match)
  (export run-dummy-server))

(select-module dummy-server)

(debug-print-width 4096)

(define (run-dummy-server port)
  (let1 pid (sys-fork)
    (if (= pid 0)
        (s3-server port)
        pid)))

(define (s3-server port)
  (let1 ssock (make-server-socket 'inet port :reuse-addr? #t)
    (let loop ()
      (let* ((csock (socket-accept ssock))
             (cin   (socket-input-port csock))
             (cout  (socket-output-port csock)))
        (let* ((stat-line (read-line cin))
               (hdrs      (rfc822-read-headers cin)))
          (match-let1 (method path ver) (string-tokenize stat-line)
            (let1 maybe-body (let1 clen (assoc-get hdrs "content-length")
                               (and clen (read-block clen cin) #f))
              ;; return data
              )))))))

(provide "dummy-server")