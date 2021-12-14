(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun parse-polymer (lines)
  (let ((polymer NIL))
    (dotimes (i (length (first lines)))
      (push (elt (first lines) (1- (- (length (first lines)) i))) polymer))
    polymer))

(defun parse-rules (lines)
  (let ((rules (make-hash-table :test #'equal)))
    (dolist (rule (cddr lines))
      (let ((split (cl-utilities:split-sequence #\Space rule)))
        (setf (gethash (first split) rules) (elt (third split) 0))))
    rules))

(defun polymer-step (polymer rules)
  (do ((i 0 (+ i 2)) (pair (make-array 2 :element-type 'character)))
      ((>= i (1- (length polymer))) polymer)
    (setf (elt pair 0) (elt polymer i))
    (setf (elt pair 1) (elt polymer (1+ i)))
    (push (gethash pair rules) (cdr (nthcdr i polymer)))))

(defun step-n-times (polymer rules n)
  (dotimes (i n)
    (polymer-step polymer rules))
  polymer)

(defun get-answer-part1 (polymer)
  (let ((times-map (make-hash-table)))
    (dolist (c polymer)
      (if (not (gethash c times-map))
          (setf (gethash c times-map) 1)
          (incf (gethash c times-map))))
    ;; (maphash #'(lambda (k v)
    ;;              (if (eql v (loop for v being the hash-values in times-map
    ;;                               minimize v))
    ;;                  (return-from least-common k))) times-map)
    (- (loop for v being the hash-values in times-map
             maximize v)
       (loop for v being the hash-values in times-map
             minimize v))))

;(print (get-answer-part1 (step-n-times (parse-polymer (get-input-lines)) (parse-rules (get-input-lines)) 10)))

(defun parse-pairs (polymer-template rules)
  (let ((pairs (make-hash-table :test #'equal)))
    (dotimes (i (1- (length polymer-template)))
      (if (not (gethash (subseq polymer-template i (+ i 2)) pairs))
          (setf (gethash (subseq polymer-template i (+ i 2)) pairs) 0))
      (incf (gethash (subseq polymer-template i (+ i 2)) pairs)))
    (maphash #'(lambda (k v)
                 (if (not (gethash k pairs)) (setf (gethash k pairs) 0)))
             rules)
    pairs))

(defun add-pairs (pair pairs rules new-pairs)
  (if (eql 0 (gethash pair pairs))
      (return-from add-pairs))
  (let ((to-add (gethash pair rules))
         (left-pair (make-array 2 :element-type 'character))
         (right-pair (make-array 2 :element-type 'character)))
    (setf (elt left-pair 0) (elt pair 0))
    (setf (elt left-pair 1) to-add)
    (setf (elt right-pair 0) to-add)
    (setf (elt right-pair 1) (elt pair 1))
    (incf (gethash left-pair new-pairs) (gethash pair pairs))
    (incf (gethash right-pair new-pairs) (gethash pair pairs))))

(defun pair-step (pairs rules)
  (let ((new-pairs (make-hash-table :test #'equal)))
    (maphash #'(lambda (k v) (setf (gethash k new-pairs) 0)) rules)
    (maphash #'(lambda (k v) (add-pairs k pairs rules new-pairs)) pairs)
    new-pairs))

(defun pairs->list (pairs)
  ;(display-map pairs)
  (let ((res NIL))
    (maphash #'(lambda (k v)
                 (dotimes (i v)
                   (push (elt k 0) res)
                   (push (elt k 1) res))) pairs)
    res))

(defun pairs->letters (pairs)
  (let ((letters (make-hash-table)))
    (maphash #'(lambda (k v)
                 (when (not (eql 0 v))
                   (if (not (gethash (elt k 0) letters))
                       (setf (gethash (elt k 0) letters) 0))
                   (incf (gethash (elt k 0) letters) v)
                   (if (not (gethash (elt k 1) letters))
                       (setf (gethash (elt k 1) letters) 0))
                   (incf (gethash (elt k 1) letters) v)))
             pairs)
    letters))

(defun pair-step-n-times (pairs rules n)
  (dotimes (i n)
    (setf pairs (pair-step pairs rules)))
  pairs)

(defun display-map (hm)
  (maphash #'(lambda (k v) (format t "~a -> ~a~%" k v)) hm))

; Subtract those and divide by 2 rounding down
(let ((letters (pairs->letters (pair-step-n-times
                                (parse-pairs (first (get-input-lines)) (parse-rules (get-input-lines)))
                                (parse-rules (get-input-lines))
                                40))))
  (print (loop for v being the hash-values in letters
               maximize v))
  (print (loop for v being the hash-values in letters
               minimize v)))
