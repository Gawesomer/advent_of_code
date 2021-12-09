(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun parse-heightmap (input-lines)
  (let* ((heightmap (make-array (list (length input-lines) (length (first input-lines))))))
    (dotimes (row (length input-lines))
      (dotimes (col (length (first input-lines)))
        (setf (aref heightmap row col) (digit-char-p (elt (elt input-lines row) col)))))
    heightmap))

(defun neighbour-coordinates(row col height width)
  (if (and (eql row 0) (eql col 0)) ; top left
      (return-from neighbour-coordinates (list (list (1+ row) col) (list row (1+ col)))))
  (if (and (eql row (1- height)) (eql 0 col)) ; bottom left
      (return-from neighbour-coordinates (list (list (1- row) col) (list row (1+ col)))))
  (if (and (eql row 0) (eql col (1- width))) ; top right
      (return-from neighbour-coordinates (list (list (1+ row) col) (list row (1- col)))))
  (if (and (eql row (1- height)) (eql col (1- width))) ; bottom right
      (return-from neighbour-coordinates (list (list (1- row) col) (list row (1- col)))))
  (if (eql row 0) ; top row
      (return-from neighbour-coordinates (list (list (1+ row) col) (list row (1- col)) (list row (1+ col)))))
  (if (eql row (1- height)) ; bottom row
      (return-from neighbour-coordinates (list (list row (1- col)) (list row (1+ col)) (list (1- row) col))))
  (if (eql col 0) ; left column
      (return-from neighbour-coordinates (list (list (1- row) col) (list (1+ row) col) (list row (1+ col)))))
  (if (eql col (1- width)) ; right column
      (return-from neighbour-coordinates (list (list (1- row) col) (list (1+ row) col) (list row (1- col)))))
  (list (list (1- row) col)
        (list (1+ row) col)
        (list row (1- col))
        (list row (1+ col))))

(defun is-low-point (row col heightmap)
  (let ((val (aref heightmap row col)))
    (dolist (neighbour (neighbour-coordinates row col (array-dimension heightmap 0) (array-dimension heightmap 1)))
      (if (>= val (aref heightmap (first neighbour) (second neighbour)))
          (return-from is-low-point NIL)))
    T))

(defun find-low-points (heightmap)
  (let ((points NIL))
    (dotimes (row (array-dimension heightmap 0))
      (dotimes (col (array-dimension heightmap 1))
        (if (is-low-point row col heightmap)
            (push (aref heightmap row col) points))))
    points))

(print (reduce #'+ (find-low-points (parse-heightmap (get-input-lines))) :key #'(lambda (e) (1+ e))))
