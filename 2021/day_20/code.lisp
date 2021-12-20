(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun parse-image (lines)
  (let ((image (make-array (list (length lines) (length (first lines))))))
    (dotimes (row (array-dimension image 0))
      (dotimes (col (array-dimension image 1))
        (setf (aref image row col) (if (eql (elt (elt lines row) col) #\.) 0 1))))
    image))

(defparameter *infinite-pixel* 0)

(defun get-pixel (image row col)
  (if (or (< row 0) (< col 0) (>= row (array-dimension image 0)) (>= col (array-dimension image 1)))
      (return-from get-pixel *infinite-pixel*)
      (aref image row col)))

(defun pixel-out (image row col rules)
  (let ((res 0))
    (dotimes (i 3)
      (dotimes (j 3)
        (setf res (ash res 1))
        (incf res (get-pixel image (+ (1- row) i) (+ (1- col) j)))))
    (if (eql (elt rules res) #\.) 0 1)))

(defun enhance (image rules)
  (let ((enhanced (make-array (list (+ (array-dimension image 0) 2) (+ (array-dimension image 1) 2)))))
    (dotimes (row (array-dimension enhanced 0))
      (dotimes (col (array-dimension enhanced 1))
        (setf (aref enhanced row col) (pixel-out image (1- row) (1- col) rules))))
    enhanced))

(defun count-lit (image)
  (let ((res 0))
    (dotimes (row (array-dimension image 0))
      (dotimes (col (array-dimension image 1))
        (if (eql (aref image row col) 1)
            (incf res))))
    res))

(let ((rules (first (get-input-lines))) (image (parse-image (cddr (get-input-lines)))))
  (dotimes (i 50)
    (setf *infinite-pixel* (mod i 2))
    (setf image (enhance image rules)))
  (print (count-lit image)))
