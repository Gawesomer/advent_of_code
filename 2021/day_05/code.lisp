(defclass line-segment ()
  ((x1
    :initarg :x1
    :reader x1)
   (y1
    :initarg :y1
    :reader y1)
   (x2
    :initarg :x2
    :reader x2)
   (y2
    :initarg :y2
    :reader y2)))

(defmethod print-object ((line line-segment) stream)
  (print-unreadable-object (line stream :type NIL)
    (format stream "~a,~a -> ~a,~a" (x1 line) (y1 line) (x2 line) (y2 line))))

(defmethod points ((line line-segment))
  (if (eql (x1 line) (x2 line))
      (return-from points (do ((i (min (y1 line) (y2 line)) (1+ i))
                               (res NIL))
                              ((> i (max (y1 line) (y2 line))) res)
                            (push (list :x (x1 line) :y i) res))))
  (if (eql (y1 line) (y2 line))
      (return-from points (do ((i (min (x1 line) (x2 line)) (1+ i))
                               (res NIL))
                              ((> i (max (x1 line) (x2 line))) res)
                            (push (list :x i :y (y1 line)) res))))
  (do ((x (x1 line) (if (> (x2 line) x)
                        (1+ x)
                        (1- x)))
       (y (y1 line) (if (> (y2 line) y)
                        (1+ y)
                        (1- y)))
       (res NIL))
      ((eql x (x2 line)) (push (list :x (x2 line) :y (y2 line)) res))
    (push (list :x x :y y) res)))

(defun parse-line-segment (input-line)
  (let* ((split-line (cl-utilities:split-sequence #\, input-line))
         (splitter-line (cl-utilities:split-sequence #\Space (second split-line))))
    (make-instance 'line-segment
                   :x1 (parse-integer (first split-line))
                   :y1 (parse-integer (first splitter-line))
                   :x2 (parse-integer (third splitter-line))
                   :y2 (parse-integer (third split-line)))))

(defun parse-line-segments ()
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-line-segment line))))

(defun max-x (lines)
  (reduce #'(lambda (curr-max line) (max curr-max (x1 line) (x2 line))) lines :initial-value 0))

(defun max-y (lines)
  (reduce #'(lambda (curr-max line) (max curr-max (y1 line) (y2 line))) lines :initial-value 0))

(defun count-overlapping (lines)
  (let* ((arr-width (1+ (max-x lines)))
         (arr-height (1+ (max-y lines)))
         (arr (make-array (list arr-width arr-height) :initial-element 0))
         (overlap 0))
    (dolist (line lines)
      (dolist (point (points line))
        (incf (aref arr (getf point :x) (getf point :y)))))
    (dotimes (x arr-width)
      (dotimes (y arr-height)
        (if (> (aref arr x y) 1)
            (incf overlap))))
    overlap))

(print (count-overlapping (parse-line-segments)))
