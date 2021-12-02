(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect (let ((line-length (length line)))
                    (list :direction (subseq line 0 (- line-length 2)) :delta (parse-integer (subseq line (1- line-length))))))))

(defun navigate (directions)
  (let ((horizontal 0) (depth 0) (aim 0))
  (dolist (instruction directions)
    (print instruction)
    (cond
      ((string-equal "up" (getf instruction :direction))
       (setf aim (- aim (getf instruction :delta))))
      ((string-equal "down" (getf instruction :direction))
       (setf aim (+ aim (getf instruction :delta))))
      ((string-equal "forward" (getf instruction :direction))
       (setf depth (+ depth (* aim (getf instruction :delta))))
       (setf horizontal (+ horizontal (getf instruction :delta))))))
    (print horizontal)
    (print depth)))

(navigate (get-input-lines))
