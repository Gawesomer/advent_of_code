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
  (print polymer))

(defun get-answer (polymer)
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

;(print (parse-polymer (get-input-lines)))
(print (get-answer (step-n-times (parse-polymer (get-input-lines)) (parse-rules (get-input-lines)) 10)))
;(maphash #'(lambda (k v) (format t "~a -> ~a~%" k v)) (parse-rules (get-input-lines)))
