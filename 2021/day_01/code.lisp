(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-integer line))))

(defun times-increased (depths)
  (let ((res 0))
  (dotimes (i (- (list-length depths) 1))
    (if (> (nth (+ i 1) depths) (nth i depths))
        (incf res)))
    (print res)))

(times-increased (get-input-lines))

(defun times-increased-triple (depths)
  (let ((res 0))
    (dotimes (i (- (list-length depths) 3))
      (let ((curr (+ (nth i depths) (nth (+ i 1) depths) (nth (+ i 2) depths)))
            (next (+ (nth (+ i 1) depths) (nth (+ i 2) depths) (nth (+ i 3) depths))))
        (if (> next curr)
            (incf res))))
    (print res)))

(times-increased-triple (get-input-lines))
