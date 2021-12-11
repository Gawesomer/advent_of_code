(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun parse-octopuses (lines)
  (let ((octopuses (make-array '(10 10))))
    (dotimes (row 10)
      (dotimes (col 10)
        (setf (aref octopuses row col) (digit-char-p (elt (elt lines row) col)))))
    octopuses))

(defun neighbour-coordinates (row col height width)
  (if (and (eql row 0) (eql col 0)) ; top left
      (return-from neighbour-coordinates (list (list (1+ row) col) (list row (1+ col)) (list (1+ row) (1+ col)))))
  (if (and (eql row (1- height)) (eql 0 col)) ; bottom left
      (return-from neighbour-coordinates (list (list (1- row) col) (list row (1+ col)) (list (1- row) (1+ col)))))
  (if (and (eql row 0) (eql col (1- width))) ; top right
      (return-from neighbour-coordinates (list (list (1+ row) col) (list row (1- col)) (list (1+ row) (1- col)))))
  (if (and (eql row (1- height)) (eql col (1- width))) ; bottom right
      (return-from neighbour-coordinates (list (list (1- row) col) (list row (1- col)) (list (1- row) (1- col)))))
  (if (eql row 0) ; top row
      (return-from neighbour-coordinates (list (list (1+ row) col) (list row (1- col)) (list row (1+ col)) (list (1+ row) (1- col)) (list (1+ row) (1+ col)))))
  (if (eql row (1- height)) ; bottom row
      (return-from neighbour-coordinates (list (list row (1- col)) (list row (1+ col)) (list (1- row) col) (list (1- row) (1- col)) (list (1- row) (1+ col)))))
  (if (eql col 0) ; left column
      (return-from neighbour-coordinates (list (list (1- row) col) (list (1+ row) col) (list row (1+ col)) (list (1- row) (1+ col)) (list (1+ row) (1+ col)))))
  (if (eql col (1- width)) ; right column
      (return-from neighbour-coordinates (list (list (1- row) col) (list (1+ row) col) (list row (1- col)) (list (1- row) (1- col)) (list (1+ row) (1- col)))))
  (list (list (1- row) col)
        (list (1+ row) col)
        (list row (1- col))
        (list row (1+ col))
        (list (1- row) (1- col))
        (list (1- row) (1+ col))
        (list (1+ row) (1- col))
        (list (1+ row) (1+ col))))

(defun octopus-flash (row col octopuses)
  (if (not (eql (aref octopuses row col) 10))
      (return-from octopus-flash 0))
  (incf (aref octopuses row col))
  (let ((num-flashes 1))
    (dolist (neighbour (neighbour-coordinates row col 10 10))
      (when (not (eql (aref octopuses (first neighbour) (second neighbour)) 10))
        (incf (aref octopuses (first neighbour) (second neighbour)))
        (incf num-flashes (octopus-flash (first neighbour) (second neighbour) octopuses))))
    num-flashes))

(defun light-step (octopuses)
  (let ((num-flashes 0))
    (dotimes (row 10)
      (dotimes (col 10)
        (incf (aref octopuses row col))))
    (dotimes (row 10)
      (dotimes (col 10)
        (incf num-flashes (octopus-flash row col octopuses))))
    (dotimes (row 10)
      (dotimes (col 10)
        (if (> (aref octopuses row col) 9)
            (setf (aref octopuses row col) 0))))
  num-flashes))

(defun step-n-times (octopuses n)
  (let ((num-flashes 0))
    (dotimes (i n)
      (incf num-flashes (light-step octopuses)))
    num-flashes))

(print (step-n-times (parse-octopuses (get-input-lines)) 100))
