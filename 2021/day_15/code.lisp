(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun parse-cave (lines)
  (let ((cave (make-array (list (length lines) (length (first lines))))))
    (dotimes (row (length lines))
      (dotimes (col (length (elt lines row)))
        (setf (aref cave row col) (digit-char-p (elt (elt lines row) col)))))
    cave))

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

(defparameter *infinity* (expt 999 999))

(defun matrix-min (matrix to-ignore)
  (let ((curr NIL) (curr-min *infinity*))
    (dotimes (row (array-dimension matrix 0))
      (dotimes (col (array-dimension matrix 1))
        (when (and (not (hash-set:hs-memberp to-ignore (list row col)))
                   (< (aref matrix row col) curr-min))
          (setf curr (list row col))
          (setf curr-min (aref matrix row col)))))
    curr))

(defun dijkstra (cave)
  (let ((distances (make-array (list (array-dimension cave 0) (array-dimension cave 1)))))
    (dotimes (row (array-dimension distances 0))
      (dotimes (col (array-dimension distances 1))
        (setf (aref distances row col) *infinity*)))
    (setf (aref distances 0 0) 0)
    (do ((spt-set (hash-set:make-hash-set)))
        ((eql (hash-set:hs-count spt-set) (* (array-dimension cave 0) (array-dimension cave 1))) distances)
      (let ((curr (matrix-min distances spt-set)))
        (hash-set:hs-ninsert spt-set curr)
        (dolist (neighbour (neighbour-coordinates (first curr) (second curr) (array-dimension cave 0) (array-dimension cave 1)))
          (if (< (+ (aref distances (first curr) (second curr)) (aref cave (first neighbour) (second neighbour)))
                 (aref distances (first neighbour) (second neighbour)))
              (setf (aref distances (first neighbour) (second neighbour))
                    (+ (aref distances (first curr) (second curr)) (aref cave (first neighbour) (second neighbour))))))))))

(defun expand-cave (cave)
  (let ((expanded (make-array (list (* 5 (array-dimension cave 0)) (* 5 (array-dimension cave 1))))))
    (dotimes (row (array-dimension cave 0))
      (dotimes (col (array-dimension cave 1))
        (dotimes (tile-row 5)
          (dotimes (tile-col 5)
            (setf (aref expanded
                        (+ row (* (array-dimension cave 0) tile-row))
                        (+ col (* (array-dimension cave 1) tile-col)))
                  (1+ (mod (1- (+ (aref cave row col) (+ tile-row tile-col))) 9)))))))
    expanded))

(defparameter *cave-size* 500)
(defparameter *min-distances* (make-array (list *cave-size* *cave-size*)))
(dotimes (row *cave-size*)
  (dotimes (col *cave-size*)
    (setf (aref *min-distances* row col) NIL)))

(defun min-path (cave position)
  (if (tree-equal position (list 0 0))
      (return-from min-path 0))
  (if (aref *min-distances* (first position) (second position))
      (return-from min-path (aref *min-distances* (first position) (second position))))
  (let ((options NIL))
    (if (> (first position) 0)
        (push (min-path cave (list (1- (first position)) (second position))) options))
    (if (> (second position) 0)
        (push (min-path cave (list (first position) (1- (second position)))) options))
    (setf (aref *min-distances* (first position) (second position)) (+ (reduce #'min options) (aref cave (first position) (second position))))
    (return-from min-path (aref *min-distances* (first position) (second position)))))

;(print (parse-cave (get-input-lines)))
;(print (dijkstra (parse-cave (get-input-lines))))
;(print (min-path (parse-cave (get-input-lines)) (list (1- *cave-size*) (1- *cave-size*))))
(print (min-path (expand-cave (parse-cave (get-input-lines))) (list (1- *cave-size*) (1- *cave-size*))))
