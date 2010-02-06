;;;
;;; Test net.amazon.aws
;;;

(use gauche.test)

(test-start "net.amazon.s3")
(use net.amazon.s3)
(test-module 'net.amazon.s3)

;; Test string-to-sign
;; Examples taken from http://docs.amazonwebservices.com/AmazonS3/2006-03-01/index.html?RESTAuthentication.html#ConstructingTheAuthenticationHeader

(define (test-sig verb headers bucket resource string signature)
  (define secret "uV3F3YluFJax1cknvbcGwgjvx4QpvB+leU8dUj2o")
  (test* #`"string-to-sign ,verb ,resource" string
         (aws-s3-string-to-sign verb headers bucket resource #f))
  (test* #`"signature ,verb ,resource" signature
         (aws-s3-signature verb headers bucket resource :secret secret)))

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

;; epilogue
(test-end)
