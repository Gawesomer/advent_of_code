(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun parse-points (lines)
  (let ((points NIL))
    (dolist (line lines)
      (if (equal line "")
          (return-from parse-points points))
      (push (map 'list #'parse-integer (cl-utilities:split-sequence #\, line)) points))))

(defun parse-instructions (lines)
  (do ((i (1+ (position "" lines :test #'equal)) (1+ i)) (instructions NIL))
      ((eql i (length lines)) (reverse instructions))
    (let ((third-column
            (cl-utilities:split-sequence #\=
                                         (third (cl-utilities:split-sequence #\Space (elt lines i))))))
      (push (list (first third-column) (parse-integer (second third-column))) instructions))))

(defun fold-vertically (points axis)
  (format t "x: ~a~%" axis)
  (dotimes (i (length points))
    (when (> (first (elt points i)) axis)
      (decf (first (elt points i)) (* 2 (- (first (elt points i)) axis)))))
  (remove-duplicates points :test #'tree-equal))

(defun fold-horizontally (points axis)
  (format t "y: ~a~%" axis)
  (dotimes (i (length points))
    (when (> (second (elt points i)) axis)
      (decf (second (elt points i)) (* 2 (- (second (elt points i)) axis)))))
  (remove-duplicates points :test #'tree-equal))

(defun fold-origami (points instructions)
  (dolist (instruction instructions)
    (if (equal (first instruction) "x")
        (setf points (fold-vertically points (second instruction)))
        (setf points (fold-horizontally points (second instruction)))))
  (display-points points))

(defun display-points (points)
  (dotimes (row (1+ (reduce #'max points :key #'second)))
    (dotimes (col (1+ (reduce #'max points :key #'first)))
      (if (find (list col row) points :test #'tree-equal)
          (format t "#")
          (format t ".")))
    (format t "~%")))

;(print (parse-points (get-input-lines)))
;(print (parse-instructions (get-input-lines)))
(fold-origami (parse-points (get-input-lines)) (parse-instructions (get-input-lines)))
