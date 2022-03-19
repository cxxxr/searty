(in-package :searty)

(defstruct inverted-value
  document-id
  positions)

(defstruct inverted-index
  (map (make-hash-table :test 'equal)))

(defun clear-inverted-index (inverted-index)
  (clrhash (inverted-index-map inverted-index)))

(defun merge-positions (positions1 positions2)
  (merge 'list positions1 positions2 #'<))

(defun merge-inverted-value (inverted-values document-id pos)
  (dolist (inverted-value inverted-values)
    (when (equal document-id (inverted-value-document-id inverted-value))
      (setf #1=(inverted-value-positions inverted-value)
            (merge-positions (list pos) #1#))
      (return-from merge-inverted-value inverted-values)))
  (cons (make-inverted-value :document-id document-id :positions (list pos))
        inverted-values))

(defun insert-inverted-value (inverted-index token-id document-id pos)
  (let ((inverted-values #1=(gethash token-id (inverted-index-map inverted-index))))
    (setf #1#
          (merge-inverted-value inverted-values document-id pos)))
  (values))

(defun inverted-index-get (inverted-index token-id)
  (gethash token-id (inverted-index-map inverted-index)))

(defun (setf inverted-index-get) (inverted-values inverted-index token-id)
  (setf (gethash token-id (inverted-index-map inverted-index))
        inverted-values))

(defmacro do-inverted-index (((token-id inverted-values) inverted-index) &body body)
  `(maphash (lambda (,token-id ,inverted-values)
              ,@body)
            (inverted-index-map ,inverted-index)))

(defun inverted-index-keys (inverted-index)
  (hash-table-keys (inverted-index-map inverted-index)))

(defun merge-inverted-values (destination-values source-values)
  (dolist (source-inverted-value source-values)
    (if-let ((destination-inverted-value
              (find (inverted-value-document-id source-inverted-value)
                    destination-values
                    :test #'equal
                    :key #'inverted-value-document-id)))
      (setf (inverted-value-positions destination-inverted-value)
            (merge-positions (inverted-value-positions destination-inverted-value)
                             (inverted-value-positions source-inverted-value)))
      (push source-inverted-value
            destination-values)))
  destination-values)

(defun merge-inverted-index (destination source)
  (do-inverted-index ((token-id inverted-values) source)
    (setf (inverted-index-get destination token-id)
          (merge-inverted-values (inverted-index-get destination token-id)
                                 inverted-values)))
  destination)

;;; encode/decode
(defun encode-positive-integer (v stream)
  (write-byte (logand v #x7f) stream)
  (loop
    (setf v (ash v -7))
    (unless (plusp v) (return))
    (write-byte (+ #x80 (logand v #x7f)) stream)))

(defun encode-positive-integer-list (integers stream)
  (encode-positive-integer (length integers) stream)
  (dolist (integer integers)
    (encode-positive-integer integer stream)))

(defun encode-uuid (uuid stream)
  (assert (and (stringp uuid) (= 36 (length uuid))))
  (loop :for c :across uuid
        :unless (char= c #\-)
        :do (write-byte (char-code c) stream)))

(defun encode-inverted-value (inverted-value stream)
  (encode-uuid (inverted-value-document-id inverted-value) stream)
  (encode-positive-integer-list (inverted-value-positions inverted-value) stream))

(defun encode-inverted-values (inverted-values stream)
  (encode-positive-integer (length inverted-values) stream)
  (dolist (inverted-value inverted-values)
    (encode-inverted-value inverted-value stream)))

(defun encode-inverted-values-to-vector (inverted-values)
  (with-open-stream (stream (flex:make-in-memory-output-stream))
    (encode-inverted-values inverted-values stream)
    (flex:get-output-stream-sequence stream)))

(defun decode-positive-integer (stream)
  (let ((v 0))
    (loop :for byte := (read-byte stream)
          :for continue := (logbitp 7 byte)
          :do (setf v
                    (logior (ash v 7)
                            (if continue
                                (logxor byte 128)
                                byte)))
          :while continue)
    v))

(defun decode-positive-integer-list (stream)
  (let ((length (read-byte stream)))
    (loop :repeat length
          :collect (decode-positive-integer stream))))

(defun decode-uuid (stream)
  (let ((uuid (make-array 36 :element-type '(unsigned-byte 8) :initial-element (char-code #\-))))
    (read-sequence uuid stream :start 0 :end 8)
    (read-sequence uuid stream :start 9 :end 13)
    (read-sequence uuid stream :start 14 :end 18)
    (read-sequence uuid stream :start 19 :end 23)
    (read-sequence uuid stream :start 24)
    (map 'string #'code-char uuid)))

(defun decode-inverted-value (stream)
  (let ((document-id (decode-uuid stream))
        (positions (decode-positive-integer-list stream)))
    (make-inverted-value :document-id document-id
                         :positions positions)))

(defun decode-inverted-values (stream)
  (let ((length (decode-positive-integer stream)))
    (loop :repeat length
          :collect (decode-inverted-value stream))))

(defun decode-inverted-values-from-vector (bytes)
  (with-open-stream (stream (flex:make-in-memory-input-stream bytes))
    (decode-inverted-values stream)))
