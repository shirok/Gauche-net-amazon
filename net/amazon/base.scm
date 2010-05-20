;;;
;;; Amazon Web Services interface common stuff
;;;

;; Usually the user program doesn't need to use this module directly,
;; for other service-specific modules (e.g. net.amazon.s3) extend this.

(define-module net.amazon.base
  (use gauche.parameter)
  (use file.util)
  (export <aws-error>
          <aws-invalid-parameter>
          <aws-net-error>
          <aws-parse-error>
          <aws-server-error>
          aws-access-key-id
          aws-secret-access-key
          aws-read-config
          aws-keys-ready?))
(select-module net.amazon.base)

(define-condition-type <aws-error> <error> #f)
(define-condition-type <aws-invalid-parameter> <aws-error> #f)
(define-condition-type <aws-net-error> <aws-error> #f
  (cause))
(define-condition-type <aws-parse-error> <aws-error> #f
  (status) (headers) (body) (cause))
(define-condition-type <aws-server-error> <aws-error> #f
  (status) (headers) (body))

(define aws-access-key-id     (make-parameter #f))
(define aws-secret-access-key (make-parameter #f))

;; A convenience function to read a common configuration file.
;; Configuration file must contain a series of S-expressions of
;; the following form:
;;   (<key> <value>)
;; where <key> is a symbol and <value> is typically a string.
;;
;; This reads the specified file (default by ~/.awsconfig),
;; recognizes aws-access-key-id and aws-secret-accses-key and
;; sets up those parameters if they exist,
;; then returns the content of the config file.

(define (aws-read-config :optional (config-file #f))
  (let* ([cfile (or config-file (build-path (home-directory) ".awsconfig"))]
         [params (or (file->sexp-list cfile :if-does-not-exist #f) '())])
    (and-let* ([p (assq 'aws-access-key-id params)])
      (aws-access-key-id (cadr p)))
    (and-let* ([p (assq 'aws-secret-access-key params)])
      (aws-secret-access-key (cadr p)))
    params))

;; A convenience function to check if keys are set up.

(define (aws-keys-ready?)
  (and (aws-access-key-id)
       (aws-secret-access-key)
       #t))
