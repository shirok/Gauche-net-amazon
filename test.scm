;;;
;;; Test net.amazon.aws
;;;

(use gauche.test)
(use gauche.parameter) ;; parameterize
(use srfi-13) ;; string-trim-both
(use rfc.822) ;; rfc822-read-headers
(use sxml.ssax) ;; ssax:xml->sxml
(use srfi-1) ;; remove
(use dummy-server) ;; run-dummy-server

(test-start "net.amazon.s3")
(use net.amazon.s3)
(test-module 'net.amazon.s3)

;; Test string-to-sign
;; Examples taken from http://docs.amazonwebservices.com/AmazonS3/2006-03-01/index.html?RESTAuthentication.html#ConstructingTheAuthenticationHeader

(define (test-sig verb headers bucket resource string signature)
  (define secret "uV3F3YluFJax1cknvbcGwgjvx4QpvB+leU8dUj2o")
  (test* #`"string-to-sign ,verb ,resource" string
         (s3-string-to-sign verb headers bucket resource #f))
  (test* #`"signature ,verb ,resource" signature
         (s3-signature verb headers bucket resource :secret secret)))

(test-sig "GET" '(("Host" "johnsmith.s3.amazonaws.com")
                  ("Date" "Tue, 27 Mar 2007 19:36:42 +0000"))
          "johnsmith" "/photos/puppy.jpg"
          "GET\n\
           \n\
           \n\
           Tue, 27 Mar 2007 19:36:42 +0000\n\
           /johnsmith/photos/puppy.jpg"
          "xXjDGYUmKxnwqr5KXNPGldn5LbA=")

(test-sig "PUT" '(("Content-Type" "image/jpeg")
                  ("Content-Length" "94328")
                  ("Host" "johnsmith.s3.amazonaws.com")
                  ("Date" "Tue, 27 Mar 2007 21:15:45 +0000"))
          "johnsmith" "/photos/puppy.jpg"
          "PUT\n\
           \n\
           image/jpeg\n\
           Tue, 27 Mar 2007 21:15:45 +0000\n\
           /johnsmith/photos/puppy.jpg"
          "hcicpDDvL9SsO6AkvxqmIWkmOuQ=")

(test-sig "GET" '(("User-Agent" "Mozilla/5.0")
                  ("Host" "johnsmith.s3.amazonaws.com")
                  ("Date" "Tue, 27 Mar 2007 19:42:41 +0000"))
          "johnsmith" "/?prefix=photos&max-keys=50&marker=puppy"
          "GET\n\
           \n\
           \n\
           Tue, 27 Mar 2007 19:42:41 +0000\n\
           /johnsmith/"
          "jsRt/rhG+Vtp88HrYL706QhE4w4=")

(test-sig "GET" '(("Host" "johnsmith.s3.amazonaws.com")
                  ("Date" "Tue, 27 Mar 2007 19:44:46 +0000"))
          "johnsmith" "/?acl"
          "GET\n\
           \n\
           \n\
           Tue, 27 Mar 2007 19:44:46 +0000\n\
           /johnsmith/?acl"
          "thdUi9VAkzhkniLj96JIrOPGi0g=")

(test-sig "DELETE" '(("User-Agent" "dotnet")
                     ("Host" "s3.amazonaws.com")
                     ("Date" "Tue, 27 Mar 2007 21:20:27 +0000")
                     ("x-amz-date" "Tue, 27 Mar 2007 21:20:26 +0000"))
          #f "/johnsmith/photos/puppy.jpg"
          "DELETE\n\
           \n\
           \n\
           \n\
           x-amz-date:Tue, 27 Mar 2007 21:20:26 +0000\n\
           /johnsmith/photos/puppy.jpg"
          "k3nL7gH3+PadhTEVn5Ip83xlYzk=")

(test-sig "PUT" '(("User-Agent" "curl/7.15.5")
                  ("Host" "static.johnsmith.net:8080")
                  ("Date" "Tue, 27 Mar 2007 21:06:08 +0000")
                  ("x-amz-acl" "public-read")
                  ("content-type" "application/x-download")
                  ("Content-MD5" "4gJE4saaMU4BqNR0kLY+lw==")
                  ("X-Amz-Meta-ReviewedBy" "joe@johnsmith.net")
                  ("X-Amz-Meta-ReviewedBy" "jane@johnsmith.net")
                  ("X-Amz-Meta-FileChecksum" "0x02661779")
                  ("X-Amz-Meta-ChecksumAlgorithm" "crc32")
                  ("Content-Disposition" "attachment; filename=database.dat")
                  ("Content-Encoding" "gzip")
                  ("Content-Length" "5913339"))
          "static.johnsmith.net" "/db-backup.dat.gz"
          "PUT\n\
           4gJE4saaMU4BqNR0kLY+lw==\n\
           application/x-download\n\
           Tue, 27 Mar 2007 21:06:08 +0000\n\
           x-amz-acl:public-read\n\
           x-amz-meta-checksumalgorithm:crc32\n\
           x-amz-meta-filechecksum:0x02661779\n\
           x-amz-meta-reviewedby:joe@johnsmith.net,jane@johnsmith.net\n\
           /static.johnsmith.net/db-backup.dat.gz"
          "C0FlOtU8Ylb9KDTpZqYkZPX91iI=")

(test-sig "GET" '(("Host" "s3.amazonaws.com")
                  ("Date" "Wed, 28 Mar 2007 01:29:59 +0000"))
          #f "/"
          "GET\n\
           \n\
           \n\
           Wed, 28 Mar 2007 01:29:59 +0000\n\
           /"
          "Db+gepJSUbZKwpx1FR0DLtEYoZA=")

(test-sig "GET" '(("Host" "s3.amazonaws.com")
                  ("Date" "Wed, 28 Mar 2007 01:49:49 +0000"))
          #f "/dictionary/fran%C3%A7ais/pr%c3%a9f%c3%a8re"
          "GET\n\
           \n\
           \n\
           Wed, 28 Mar 2007 01:49:49 +0000\n\
           /dictionary/fran%C3%A7ais/pr%c3%a9f%c3%a8re"
          "dxhSBHoI6eVSPcXJqEghlUzZMnY=")

(define (compare-response-raw expected raw)
  (define ns '((aws . "http://s3.amazonaws.com/doc/2006-03-01/")))
  (define (parse-response expected)
    (call-with-input-string expected
      (lambda (iport) (let* ((status (read-line iport))
                             (hdrs   (rfc822-read-headers iport))
                             (cont   (port->string iport)))
                        (values cont hdrs)))))
  (define (debug-print-sexps . lis)
    (fold (lambda (l _) (and l #?=l)) #f lis))
  (define (compare-sxml-container left right)
    (define (sxml-container sxml)
      (if (pair? sxml)
          (cons (car sxml)
                (map sxml-container
                     (sort (remove (lambda (e)
                                     (if (pair? e) (eq? (car e) '@) #t))
                                   (cdr sxml))
                           (lambda (x y) (string<? (symbol->string (car x))
                                                   (symbol->string (car y)))))))
          '()))
    (or (equal? (sxml-container left) (sxml-container right))
        (debug-print-sexps (sxml-container left)
                           (sxml-container right)
                           #f)))
  (receive (cont hdrs) (parse-response expected)
    (and (or (equal? (sort (filter #/^x-amz-/ (map car hdrs)) string<?)
                     (sort (filter #/^x-amz-/ (map car (cadr raw))) string<?))
             (debug-print-sexps
              (sort (filter #/^x-amz-/ (map car hdrs)) string<?)
              (sort (filter #/^x-amz-/ (map car (cadr raw))) string<?)
              #f))
         (if (equal? (rfc822-header-ref hdrs "content-type") "text/plain")
             (equal? (car raw) cont)
             (compare-sxml-container
              (if (or (equal? (rfc822-header-ref hdrs "content-length")
                                 "0")
                         (string=? cont ""))
                     '()
                     (call-with-input-string cont
                       (cut ssax:xml->sxml <> ns)))
              (car raw))))))

(define-macro (test-raw label expected proc)
  `(test ,label ,expected
         (lambda () (receive (sxml hdrs) ,proc (list sxml hdrs)))
         compare-response-raw))

(let1 port 8080
  (with-module net.amazon.s3 (set! *s3-endpoint* #`"localhost:,|port|"))
  (run-dummy-server port))

(let1 bucket "yfujisawa2" ;; #`",(sys-time)"
  (define (keys-file-load iport)
    (define (assoc-get lis key)
      (cond [(assoc key lis) => (lambda (e) (and (not (null? e)) (cadr e)))]
            [else #f]))
    (let1 alist (map (lambda (line)
                       (map string-trim-both (string-split line ":")))
                     (port->list read-line iport))
      (values (assoc-get alist "access-key-id")
              (assoc-get alist "secret-access-key"))))
  (receive (keyid skey) (call-with-input-file "keys.txt" keys-file-load)
    (when (and keyid skey)
      (parameterize ((aws-access-key-id     keyid)
                     (aws-secret-access-key skey))
        (test*    "bucket-list" '() (s3-bucket-list))
        (test-raw "bucket-list/raw" "HTTP/1.1 200 OK
x-amz-id-2: gyB+3jRPnrkN98ZajxHXr3u7EFM67bNgSAxexeEHndCX/7GRnfTXxReKUQF28IfP
x-amz-request-id: 3B3C7C725673C630
Date: Wed, 01 Mar  2009 12:00:00 GMT
Content-Type: application/xml
Content-Length: 302
Connection: close
Server: AmazonS3

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListAllMyBucketsResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">
  <Owner>
    <ID>bcaf1ffd86f461ca5fb16fd081034f</ID>
    <DisplayName>webfile</DisplayName>
  </Owner>
  <Buckets/>
</ListAllMyBucketsResult>"
                  (s3-bucket-list/raw))
        (test* #`"bucket-available? ,|bucket| if does not exist"
               'available
               (s3-bucket-availability bucket))
        (test* #`"create bucket ,|bucket|" #t
               (begin (s3-bucket-create! bucket) #t))
        (test* #`"bucket-available? ,|bucket| if exists"
               'used
               (s3-bucket-availability bucket))
        (test* #`"bucket-location us-classic" 'us-classic
               (s3-bucket-location bucket))
        (test-raw #`"bucket-location/raw us-classic" "HTTP/1.1 200 OK
x-amz-id-2: gyB+3jRPnrkN98ZajxHXr3u7EFM67bNgSAxexeEHndCX/7GRnfTXxReKUQF28IfP
x-amz-request-id: 3B3C7C725673C630
Date: Wed, 01 Mar  2009 12:00:00 GMT
Content-Type: application/xml
Content-Length: 302
Connection: close
Server: AmazonS3

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<LocationConstraint xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"/>"
                  (s3-bucket-location/raw bucket))
        (test* #`"delete bucket ,|bucket|" #t
               (begin (s3-bucket-delete! bucket) #t))

        (let1 bucket #`",|bucket|2"
          (test-raw #`"create-bucket/raw! ,|bucket|" "HTTP/1.1 200 OK
x-amz-id-2: YgIPIfBiKa2bj0KMg95r/0zo3emzU4dzsD4rcKCHQUAdQkf3ShJTOOpXUueF6QKo
x-amz-request-id: 236A8905248E5A01
Date: Wed, 01 Mar  2009 12:00:00 GMT
Location: /colorpictures
Content-Length: 0
Connection: close
Server: AmazonS3"
                  (s3-bucket-create/raw! bucket :location "EU"))
        (test* #`"bucket-location EU" 'EU
               (s3-bucket-location bucket))
        (test-raw #`"bucket-location/raw EU" "HTTP/1.1 200 OK
x-amz-id-2: gyB+3jRPnrkN98ZajxHXr3u7EFM67bNgSAxexeEHndCX/7GRnfTXxReKUQF28IfP
x-amz-request-id: 3B3C7C725673C630
Date: Wed, 01 Mar  2009 12:00:00 GMT
Content-Type: application/xml
Content-Length: 302
Connection: close
Server: AmazonS3

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<LocationConstraint xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">
EU
</LocationConstraint>"
                  (s3-bucket-location/raw bucket))
        (test-raw #`"object-list/raw ,|bucket|" #`"HTTP/1.1 200 OK
x-amz-id-2: gyB+3jRPnrkN98ZajxHXr3u7EFM67bNgSAxexeEHndCX/7GRnfTXxReKUQF28IfP
x-amz-request-id: 3B3C7C725673C630
Date: Wed, 01 Mar  2009 12:00:00 GMT
Content-Type: application/xml
Content-Length: 302
Connection: close
Server: AmazonS3

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">
  <Name>,|bucket|</Name>
  <Prefix/>
  <Marker/>
  <MaxKeys>1000</MaxKeys>
  <IsTruncated>false</IsTruncated>
</ListBucketResult>"
                  (s3-object-list/raw bucket))
        (test* #`"object-put! ,|bucket| test-obj.txt"
               (s3-object-put! bucket "test-obj.txt" "this is a test.")
               (rfc822-header-ref (s3-object-head bucket "test-obj.txt")
                                     "etag"))
        
        (test-raw "object-put/raw!" "HTTP/1.1 200 OK
x-amz-id-2: LriYPLdmOdAiIfgSm/F1YsViT1LW94/xUQxMsF7xiEb1a0wiIOIxl+zbwZ163pt7
x-amz-request-id: 0A49CE4060975EAC
Date: Wed, 12 Oct 2009 17:50:00 GMT
ETag: \"1b2cf535f27731c974343645a3985328\"
Content-Length: 0
Connection: close
Server: AmazonS3"
                  (s3-object-put/raw! bucket "test-obj2.txt" "this is a test."))
        (test* #`"object-get ,|bucket| test-obj.txt" "this is a test."
               (s3-object-get bucket "test-obj.txt"))
        (test-raw #`"object-get/raw ,|bucket| test-obj.txt" "HTTP/1.1 200 OK
x-amz-id-2: eftixk72aD6Ap51TnqcoF8eFidJG9Z/2mkiDFu8yU9AS1ed4OpIszj7UDNEHGran
x-amz-request-id: 318BC8BC148832E5
Date: Wed, 28 Oct 2009 22:32:00 GMT
Last-Modified: Wed, 12 Oct 2009 17:50:00 GMT
ETag: \"fba9dede5f27731c9771645a39863328\"
Content-Length: 15
Content-Type: text/plain
Connection: close
Server: AmazonS3

this is a test."
                  (s3-object-get/raw bucket "test-obj.txt"))
        (test-raw #`"object-list/raw ,|bucket|" #`"HTTP/1.1 200 OK
x-amz-id-2: gyB+3jRPnrkN98ZajxHXr3u7EFM67bNgSAxexeEHndCX/7GRnfTXxReKUQF28IfP
x-amz-request-id: 3B3C7C725673C630
Date: Wed, 01 Mar  2009 12:00:00 GMT
Content-Type: application/xml
Content-Length: 302
Connection: close
Server: AmazonS3

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">
  <Name>,|bucket|</Name>
  <Prefix/>
  <Marker/>
  <MaxKeys/>
  <IsTruncated>false</IsTruncated>
  <Contents>
    <Key>test-obj.txt</Key>
    <LastModified>2006-01-01T12:00:00.000Z</LastModified>
    <ETag>&quot;828ef3fdfa96f00ad9f27c383fc9ac7f&quot;</ETag>
    <Size>5</Size>
    <StorageClass>STANDARD</StorageClass>
    <Owner>
      <ID>bcaf161ca5fb16fd081034f</ID>
      <DisplayName>webfile</DisplayName>
     </Owner>
  </Contents>
  <Contents>
    <Key>test-obj2.txt</Key>
    <LastModified>2006-01-01T12:00:00.000Z</LastModified>
    <ETag>&quot;828ef3fdfa96f00ad9f27c383fc9ac7f&quot;</ETag>
    <Size>5</Size>
    <StorageClass>STANDARD</StorageClass>
    <Owner>
      <ID>bcaf161ca5fb16fd081034f</ID>
      <DisplayName>webfile</DisplayName>
     </Owner>
  </Contents>
</ListBucketResult>"
                  (s3-object-list/raw bucket))
        (let1 copied-etag (s3-object-copy! bucket #`"/,|bucket|/test-obj.txt"
                                           "test-obj3.txt")
          (receive (sxml hdrs) (s3-object-get/raw bucket "test-obj3.txt")
            (define (filter-token hdrs tokens)
              (let loop ((hdrs hdrs) (out '()))
                (if (null? hdrs)
                    (reverse out)
                    (if (member (caar hdrs) tokens)
                        (loop (cdr hdrs) (cons (car hdrs) out))
                        (loop (cdr hdrs) out)))))
            (test* #`"object-head ,|bucket|" (filter-token hdrs '("etag" "last-modified"))
                   (filter-token (s3-object-head bucket "test-obj3.txt")
                                 '("etag" "last-modified")))
            (test* #`"object-head/raw ,|bucket|" (cons #f (filter-token hdrs '("etag" "last-modified")))
                   (receive (sxml hdrs) (s3-object-head/raw bucket "test-obj3.txt")
                     (cons sxml (filter-token hdrs '("etag" "last-modified")))))
            (test* "object-copy!" copied-etag
                   (cond [(assoc "ETag" hdrs) => cadr]
                         [else #f]))))
        (test-raw "object-copy/raw!" "HTTP/1.1 200 OK
x-amz-id-2: eftixk72aD6Ap51TnqcoF8eFidJG9Z/2mkiDFu8yU9AS1ed4OpIszj7UDNEHGran
x-amz-request-id: 318BC8BC148832E5
Date: Wed, 28 Oct 2009 22:32:00 GMT
Connection: close
Server: AmazonS3

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<CopyObjectResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">
   <LastModified>2009-10-28T22:32:00</LastModified>
   <ETag>\"9b2cf535f27731c974343645a3985328\"</ETag>
 </CopyObjectResult>"
                  (s3-object-copy/raw! bucket #`"/,|bucket|/test-obj.txt"
                                       "test-obj4.txt"))
        (test* #`"object-list ,|bucket|" '("test-obj.txt" "test-obj2.txt"
                                           "test-obj3.txt" "test-obj4.txt")
               (s3-object-list bucket))
        (test* #`"object-delete! ,|bucket| test-obj.txt" #t
               (begin (s3-object-delete! bucket "test-obj.txt") #t))
        (test* #`"object-delete! ,|bucket| test-obj2.txt" #t
               (begin (s3-object-delete! bucket "test-obj2.txt") #t))
        (test-raw #`"object-delete/raw! ,|bucket| test-obj3.txt"
               "HTTP/1.1 204 NoContent
x-amz-id-2: LriYPLdmOdAiIfgSm/F1YsViT1LW94/xUQxMsF7xiEb1a0wiIOIxl+zbwZ163pt7
x-amz-request-id: 0A49CE4060975EAC
Date: Wed, 12 Oct 2009 17:50:00 GMT
Content-Length: 0
Connection: close
Server: AmazonS3"
               (s3-object-delete/raw! bucket "test-obj3.txt"))
        (test* #`"object-delete! ,|bucket| test-obj4.txt" #t
               (begin (s3-object-delete! bucket "test-obj4.txt") #t))
        (test* #`"object-list ,|bucket|" '()
               (s3-object-list bucket))
        (test-raw #`"delete-bucket/raw! ,|bucket|" "HTTP/1.1 204 No Content
x-amz-id-2: JuKZqmXuiwFeDQxhD7M8KtsKobSzWA1QEjLbTMTagkKdBX2z7Il/jGhDeJ3j6s80
x-amz-request-id: 32FE2CEB32F5EE25
Date: Wed, 01 Mar  2009 12:00:00 GMT
Connection: close
Server: AmazonS3"
                  (s3-bucket-delete/raw! bucket)))
        ))))

;; epilogue
(test-end)
