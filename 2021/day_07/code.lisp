(defun parse-positions ()
  (with-open-file (in "input.txt")
    (first (loop for line = (read-line in nil nil)
          while line
          collect (map 'list 'parse-integer (cl-utilities:split-sequence #\, line))))))

;(defun fuel-to-target (target positions)
;  (reduce #'+ positions :key #'(lambda (position) (abs (- position target)))))

(defun geometric-sum (n)
  (/ (* n (1+ n)) 2))

(defun fuel-to-target (target positions)
  (reduce #'+ positions :key #'(lambda (position) (geometric-sum (abs (- position target))))))

(defun min-fuel (positions)
  (do ((i (reduce #'min positions) (1+ i)) (contender 1000000000000)) ; Infinity?
      ((> i (reduce #'max positions)) contender)
    (format t "~a: ~a~%" i (fuel-to-target i positions))
    (if (< (fuel-to-target i positions) contender)
        (setf contender (fuel-to-target i positions)))))

;(print (min-fuel (list 16 1 2 0 4 2 7 1 2 14)))
(print (min-fuel (parse-positions)))
