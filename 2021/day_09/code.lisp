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

(defun is-low-point (row col heightmap)
  (let ((val (aref heightmap row col)))
    (when (and (eql row 0) (eql col 0)) ; top left
      (if (and (< val (aref heightmap (1+ row) col)) (< val (aref heightmap row (1+ col))))
          (return-from is-low-point T)
          (return-from is-low-point NIL)))
    (when (and (eql row (1- (array-dimension heightmap 0))) (eql 0 col)) ; bottom left
      (if (and (< val (aref heightmap (1- row) col)) (< val (aref heightmap row (1+ col))))
          (return-from is-low-point T)
          (return-from is-low-point NIL)))
    (when (and (eql row 0) (eql col (1- (array-dimension heightmap 1)))) ; top right
      (if (and (< val (aref heightmap (1+ row) col)) (< val (aref heightmap row (1- col))))
          (return-from is-low-point T)
          (return-from is-low-point NIL)))
    (when (and (eql row (1- (array-dimension heightmap 0))) (eql col (1- (array-dimension heightmap 1)))) ; bottom right
      (if (and (< val (aref heightmap (1- row) col)) (< val (aref heightmap row (1- col))))
          (return-from is-low-point T)
          (return-from is-low-point NIL)))
    (when (eql row 0) ; top row
      (if (and (< val (aref heightmap (1+ row) col)) (< val (aref heightmap row (1- col))) (< val (aref heightmap row (1+ col))))
          (return-from is-low-point T)
          (return-from is-low-point NIL)))
    (when (eql row (1- (array-dimension heightmap 0))) ; bottom row
      (if (and (< val (aref heightmap row (1- col))) (< val (aref heightmap row (1+ col))) (< val (aref heightmap (1- row) col)))
          (return-from is-low-point T)
          (return-from is-low-point NIL)))
    (when (eql col 0) ; left column
      (if (and (< val (aref heightmap (1- row) col)) (< val (aref heightmap (1+ row) col)) (< val (aref heightmap row (1+ col))))
          (return-from is-low-point T)
          (return-from is-low-point NIL)))
    (when (eql col (1- (array-dimension heightmap 1))) ; right column
      (if (and (< val (aref heightmap (1- row) col)) (< val (aref heightmap (1+ row) col)) (< val (aref heightmap row (1- col))))
          (return-from is-low-point T)
          (return-from is-low-point NIL)))
    (and (< val (aref heightmap (1- row) col))
         (< val (aref heightmap (1+ row) col))
         (< val (aref heightmap row (1- col)))
         (< val (aref heightmap row (1+ col))))))

(defun find-low-points (heightmap)
  (let ((points NIL))
    (dotimes (row (array-dimension heightmap 0))
      (dotimes (col (array-dimension heightmap 1))
        (if (is-low-point row col heightmap)
            (push (aref heightmap row col) points))))
    points))

(print (reduce #'+ (find-low-points (parse-heightmap (get-input-lines))) :key #'(lambda (e) (1+ e))))
