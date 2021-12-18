(defclass packet ()
  ((bits
    :initarg :bits
    :accessor bits)
   (curr-bit
    :initform 6
    :accessor curr-bit)
   (version
    :initarg :version
    :accessor version)
   (packet-type
    :initarg :packet-type
    :accessor packet-type)
   (size
    :initarg :size
    :initform 0
    :accessor size)))

(defclass packet-literal (packet)
   ((literal
    :initform 0
    :accessor literal)))

(defmethod print-object ((pckt packet-literal) stream)
  (print-unreadable-object (pckt stream :type NIL)
    (format stream "v~a, ~a, ~abits: ~a" (version pckt) (packet-type pckt) (size pckt) (literal pckt))))

(defclass packet-operator (packet)
   ((subpackets
    :initarg :subpackets
    :initform NIL
    :accessor subpackets)))

(defmethod print-object ((pckt packet-operator) stream)
  (print-unreadable-object (pckt stream :type NIL)
    (format stream "v~a, ~a, ~abits: ~{~a~^, ~}" (version pckt) (packet-type pckt) (size pckt) (subpackets pckt))))

(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (first(loop for line = (read-line in nil nil)
          while line
          collect line))))

(defun hexstring->int (hexstring)
  (with-input-from-string (s (concatenate 'string "#x" hexstring))
    (read s)))

(defun hexchar->int (hexchar)
  (with-input-from-string (s (concatenate 'string "#x" (list hexchar)))
    (read s)))

(defun int->bitstring (i)
  (format nil "~b" i))

(defun bitstring->bitvector (bitstring)
  (with-input-from-string (s (concatenate 'string "#*" bitstring))
    (read s)))

(defun hexchar->bitvector (hexchar)
  (let ((bits (bitstring->bitvector (int->bitstring (hexchar->int hexchar)))))
    (dotimes (i (- 4 (length bits)))
      (setf bits (concatenate 'bit-vector #*0 bits)))
    bits))

; Phew that was a lot of functions to get to this
(defun hexstring->bitvector (hexstring)
  (let ((bits NIL))
    (dotimes (i (length hexstring))
      (setf bits (concatenate 'bit-vector bits (hexchar->bitvector (elt hexstring i)))))
    bits))

(defun bitvector->int (bits)
  (with-input-from-string (s (substitute #\b #\* (format nil "~a" bits) :count 1))
    (read s)))

(defun subbits (bits start len)
  (let ((bitmask 0) (bits-length (integer-length bits)))
    (setf len (min len (- bits-length start)))
    (dotimes (i len)
      (setf bitmask (ash bitmask 1))
      (incf bitmask))
    (setf bitmask (ash bitmask (- bits-length start len)))
    (ash (logand bits bitmask) (- (- bits-length start len)))))

(defun subbits-with-start-padding (bits start len)
  (let ((bitmask 0) (bits-length (integer-length bits)))
    (incf bits-length (- 4 (mod bits-length 4)))
    (setf len (min len (- bits-length start)))
    (dotimes (i len)
      (setf bitmask (ash bitmask 1))
      (incf bitmask))
    (setf bitmask (ash bitmask (- bits-length start len)))
    (ash (logand bits bitmask) (- (- bits-length start len)))))

(defun parse (bits)
  (format t "parsing: ~a~%" bits)
  (if (eql (parse-packet-type bits) 4)
      (parse-literal bits)
      (parse-operator bits)))

(defun parse-version (bits)
  (bitvector->int (subseq bits 0 3)))

(defun parse-packet-type (bits)
  (bitvector->int (subseq bits 3 6)))

(defun parse-literal (bits)
  (let ((pckt (make-instance 'packet-literal :bits bits :version (parse-version bits) :packet-type 4)))
    (parse-packet pckt)
    pckt))

(defun parse-operator (bits)
  (let ((pckt (make-instance 'packet-operator :bits bits :version (parse-version bits) :packet-type (parse-packet-type bits))))
    (parse-packet pckt)
    pckt))

(defmethod parse-packet ((pckt packet-literal))
  (with-accessors ((curr-bit curr-bit) (literal literal) (bits bits) (size size)) pckt
    (do ()
        ((eql (bitvector->int (subseq bits curr-bit (1+ curr-bit))) 0)
         (progn (incf literal (bitvector->int (subseq bits (1+ curr-bit) (+ curr-bit 5))))
                (incf curr-bit 5)
                (setf size curr-bit)
                NIL))
      (incf literal (bitvector->int (subseq bits (1+ curr-bit) (+ curr-bit 5))))
      (setf literal (ash literal 4))
      (incf curr-bit 5))))

(defmethod parse-packet ((pckt packet-operator))
  (with-accessors ((curr-bit curr-bit) (bits bits) (size size) (subpackets subpackets)) pckt
    (if (eql (bitvector->int (subseq bits curr-bit (1+ curr-bit))) 0)
        (progn
          (incf curr-bit)
          (setf size (+ 22 (bitvector->int (subseq bits curr-bit (+ curr-bit 15)))))
          (incf curr-bit 15)
          (do ()
              ((>= curr-bit size) NIL)
            (setf subpackets (append subpackets (list (parse (subseq bits curr-bit)))))
            (incf curr-bit (slot-value (car (last subpackets)) 'size))))
        (progn
          (incf curr-bit)
          (let ((num-subpackets (bitvector->int (subseq bits curr-bit (+ curr-bit 11)))))
            (incf curr-bit 11)
            (dotimes (i num-subpackets)
              (setf subpackets (append subpackets (list (parse (subseq bits curr-bit)))))
              (incf curr-bit (slot-value (car (last subpackets)) 'size)))
            (setf size curr-bit))))))

(defmethod version-sum ((pckt packet))
  (if (eql (packet-type pckt) 4)
      (version pckt)
      (+ (version pckt) (reduce #'+ (map 'list #'version-sum (subpackets pckt))))))

(defmethod evaluate ((pckt packet))
  (cond
    ((eql (packet-type pckt) 0)
     (reduce #'+ (map 'list #'evaluate (subpackets pckt))))
    ((eql (packet-type pckt) 1)
     (reduce #'* (map 'list #'evaluate (subpackets pckt))))
    ((eql (packet-type pckt) 2)
     (reduce #'min (map 'list #'evaluate (subpackets pckt))))
    ((eql (packet-type pckt) 3)
     (reduce #'max (map 'list #'evaluate (subpackets pckt))))
    ((eql (packet-type pckt) 4)
     (literal pckt))
    ((eql (packet-type pckt) 5)
     (if (> (evaluate (first (subpackets pckt))) (evaluate (second (subpackets pckt)))) 1 0))
    ((eql (packet-type pckt) 6)
     (if (< (evaluate (first (subpackets pckt))) (evaluate (second (subpackets pckt)))) 1 0))
    ((eql (packet-type pckt) 7)
     (if (eql (evaluate (first (subpackets pckt))) (evaluate (second (subpackets pckt)))) 1 0))))

(let ((pckt (parse (hexstring->bitvector (get-input-lines)))))
  (print pckt)
  (print (version-sum pckt))
  (print (evaluate pckt)))
