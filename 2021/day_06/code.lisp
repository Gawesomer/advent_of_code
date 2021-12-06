(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (first (loop for line = (read-line in nil nil)
          while line
          collect (map 'list 'parse-integer (cl-utilities:split-sequence #\, line))))))

(defun cycle (fishes)
  (dotimes (i (length fishes))
    (if (eql (nth i fishes) 0)
        (setf fishes (append fishes (list 8))))
    (decf (nth i fishes))
    (if (< (nth i fishes) 0)
        (setf (nth i fishes) 6)))
  fishes)

(defun cycle-times (fishes times)
  (dotimes (i times)
    (setf fishes (cycle fishes)))
  fishes)

;(print (length (cycle-times '(3 4 3 1 2) 80)))
;(print (length (cycle-times (get-input-lines) 80))) ; This is dumb...

(defun squash (fishes)
  (do ((i 0 (1+ i)) (res '(0 0 0 0 0 0 0 0 0)))
      ((eql i (length fishes)) res)
    (incf (nth (nth i fishes) res))))

(defun smarter-cycle-times (fishes times)
  (dotimes (i times)
    (let ((newborn-count (first fishes)))
      (dotimes (j 8)
        (setf (nth j fishes) (nth (1+ j) fishes)))
      (setf (nth 6 fishes) (+ (nth 6 fishes) newborn-count))
      (setf (nth 8 fishes) newborn-count)))
  fishes)

;(print (reduce #'+ (smarter-cycle-times (squash '(3 4 3 1 2)) 80)))
(print (reduce #'+ (smarter-cycle-times (squash (get-input-lines)) 256)))
