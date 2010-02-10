What is this
------------

sSh3ll.scm is a convenient Amazon S3 access utility. If you know rSh3ll.rb, the Ruby based Amazon access utility which is distributed in http://rubyforge.org/projects/rsh3ll/, sSh3ll.scm is similar to it.


Before run it
-------------

Please edit keys.txt and fill in access-key-id and secret-access-key. If you don't have those keys, you can obtain it from http://aws.amazon.com/s3/.


How to run it
-------------

You can run sSh3ll.scm with 'gosh -I. sSh3ll.scm'


Supported commands
------------------

* bucket [bucket]

Show and set target bucket. 

* createbucket

Crate bucket if available. The bucket is set by 'bucket [bucket]' command.

* deletebucket

Delete bucket. The bucket is set by 'bucket [bucket]' command.

* bucket-list

List buckets.

* bucket-available?

Check the bucket is available or not. The bucket is set by 'bucket [bucket]' command.

* bucket-location

Check which Region the bucket is located. About Region, please check Amazon S3 reference. The bucket is set by 'bucket [bucket]' command.

* list [prefix] [max]

List up the objects which is contained in the target bucket. The target bucket is set by 'bucket [bucket]' command.

* put <object-id> <text>

Put a text line to a target bucket. The target bucket is set by 'bucket [bucket]' command.

* get <object-id>

Get the content of object and show it. The object is containd in target bucket.

* head <object-id>

Get the HTTP header of the object and show it as S-expression. The object is containd in target bucket.

* delete <object-id>

Delete the object from the target bucket. The target bucket is set by 'bucket [bucket]' command.

* copy <source-object-bucket+id> <destination-object-id>

Copy an object between buckets. <source-object-bucket+id> is typically "/source-bucket/source-object-id" and <destination-object-id> is typically "destination-object-id". The bucket of destination is the bucke set by 'bucket [bucket]' command.
