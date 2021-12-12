(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun build-adjacency-list (lines)
  (let ((graph (make-hash-table :test #'equal)))
    (dolist (line lines)
      (let ((from (first (cl-utilities:split-sequence #\- line)))
            (to (second (cl-utilities:split-sequence #\- line))))
        (push to (gethash from graph))
        (push from (gethash to graph))))
    graph))

(defun display-graph (graph)
  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) graph))

(defun neighbours (position graph)
  (gethash position graph))

(defun visitable-neighbours (position visited graph)
  (remove-if #'(lambda (e) (and (is-small-cave e) (find e visited :test #'equal))) (neighbours position graph)))

(defun is-small-cave (position)
  (not (equal position (string-upcase position))))

(defun num-paths (position visited graph)
  (format t "in: ~a~%" position)
  (if (equal position "end")
      (return-from num-paths 1))
  (let ((res 0))
    (dolist (neighbour (visitable-neighbours position visited graph))
      (incf res (num-paths neighbour (concatenate 'list (list position) visited) graph)))
    (format t "out: ~a, ~a~%" position res)
    res))

(defparameter *joker-used* NIL)

(defun visitable-neighbours-with-jokers (position visited graph)
  (if *joker-used*
      (remove-if #'(lambda (e) (and (is-small-cave e) (find e visited :test #'equal))) (neighbours position graph))
      (remove-if #'(lambda (e) (or (equal e "start") (equal e "end"))) (neighbours position graph))))

(defun num-paths-with-jokers (position visited graph)
  (format t "in: ~a, joker: ~a~%" position *joker-used*)
  (if (equal position "end")
      (return-from num-paths-with-jokers 1))
  (let ((res 0))
    (dolist (neighbour (visitable-neighbours-with-jokers position visited graph))
      (if (and (not *joker-used*) (is-small-cave neighbour) (find neighbour visited :test #'equal))
          (progn
            (setf *joker-used* T)
            (incf res (num-paths-with-jokers neighbour (concatenate 'list (list position) visited) graph))
            (setf *joker-used* NIL))
          (incf res (num-paths-with-jokers neighbour (concatenate 'list (list position) visited) graph))))
    (format t "out: ~a, ~a~%" position res)
    res))

(print (num-paths "start" NIL (build-adjacency-list (get-input-lines))))
(print (num-paths-with-jokers "start" NIL (build-adjacency-list (get-input-lines))))
(print (+ (num-paths "start" NIL (build-adjacency-list (get-input-lines))) (num-paths-with-jokers "start" NIL (build-adjacency-list (get-input-lines)))))
