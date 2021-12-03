(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun most-common (lines bitnum)
    (let ((zero-count 0) (one-count 0))
      (dolist (line lines)
        (if (char= (elt line bitnum) #\0)
            (incf zero-count)
            (incf one-count)))
      (if (> one-count zero-count)
          (return-from most-common 1)
          0)))

(defun find-gamma (lines)
  (let ((res 0))
    (dotimes (i 12)
      (if (eql (most-common lines (- 11 i)) 1)
          (setf res (+ res (expt 2 i)))))
    (print res)))

(defun find-epsilon(lines)
  (let ((res 0))
    (dotimes (i 12)
      (if (eql (most-common lines (- 11 i)) 0)
          (setf res (+ res (expt 2 i)))))
    (print res)))

; 1616, 2479
(find-gamma (get-input-lines))
(find-epsilon (get-input-lines))
