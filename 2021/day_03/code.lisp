(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun most-common (elements &key (test #'eql))
  "Returns most common element in list"
  (do ((i 0 (1+ i))
       (occurences (make-hash-table :test test))
       (contender (list :value NIL :count 0)))
      ((eql i (length elements)) (getf contender :value))
    (let ((curr-elt (elt elements i)))
      (if (not (gethash curr-elt occurences)) ; Default dict?
          (setf (gethash curr-elt occurences) 0))
      (incf (gethash curr-elt occurences))
      (if (> (gethash curr-elt occurences) (getf contender :count))
          (setf contender (list :value curr-elt :count (gethash curr-elt occurences)))))))

(defun bitstring->int (bitstring)
  (do ((i 0 (1+ i))
       (num-bits (length bitstring))
       (int-value 0 (if (char= (elt bitstring (1- (- num-bits i))) #\1)
                        (+ int-value (expt 2 i))
                        int-value)))
      ((eql i num-bits) int-value)))

(defun most-common-bit (lines bitnum)
  (let ((bits (map 'list #'(lambda (line) (elt line bitnum)) lines)))
    (if (eql (count #\0 bits) (count #\1 bits))
        #\1
        (most-common bits))))

(defun find-gamma (lines)
  (let ((res 0))
    (dotimes (i 12)
      (if (char= (most-common-bit lines (- 11 i)) #\1)
          (setf res (+ res (expt 2 i)))))
    res))

(defun find-epsilon (lines)
  (let ((res 0))
    (dotimes (i 12)
      (if (char= (most-common-bit lines (- 11 i)) #\0)
          (setf res (+ res (expt 2 i)))))
    res))

(print (* (find-gamma (get-input-lines)) (find-epsilon (get-input-lines))))

(defun find-oxygen (lines)
  (do ((filtered lines) (i 0 (1+ i)) (common))
      ((eql (length filtered) 1) (first filtered))
    (setf common (most-common-bit filtered i))
    (setf filtered (remove-if-not #'(lambda (line) (char= common (elt line i))) filtered))))

(defun find-carbon (lines)
  (do ((filtered lines) (i 0 (1+ i)) (common))
      ((eql (length filtered) 1) (first filtered))
    (setf common (most-common-bit filtered i))
    (setf filtered (remove-if #'(lambda (line) (char= common (elt line i))) filtered))))

(print (* (bitstring->int (find-oxygen (get-input-lines))) (bitstring->int (find-carbon (get-input-lines)))))
