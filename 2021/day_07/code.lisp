(defun parse-positions ()
  (with-open-file (in "input.txt")
    (first (loop for line = (read-line in nil nil)
          while line
          collect (map 'list 'parse-integer (cl-utilities:split-sequence #\, line))))))

(defun fuel-to-target (target positions)
  (reduce #'+ positions :key #'(lambda (position) (abs (- position target)))))

(defun min-fuel (positions)
  (do ((i (reduce #'min positions) (1+ i)) (contender (* (reduce #'max positions) (length positions))))
      ((> i (reduce #'max positions)) contender)
    (if (< (fuel-to-target i positions) contender)
        (setf contender (fuel-to-target i positions)))))

(print (min-fuel (parse-positions)))
