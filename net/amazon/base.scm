;;;
;;; Amazon Web Services interface common stuff
;;;

;; Usually the user program doesn't need to use this module directly,
;; for other service-specific modules (e.g. net.amazon.s3) extend this.

(define-module net.amazon.base
  (use gauche.parameter)
  (export <aws-error>
          <aws-invalid-parameter>
          <aws-net-error>
          <aws-parse-error>
          <aws-server-error>
          aws-access-key-id
          aws-secret-access-key))
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
