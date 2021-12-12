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

(defun non-visited-neighbours (position visited graph)
  (remove-if #'(lambda (e) (and (is-small-cave e) (find e visited :test #'equal))) (neighbours position graph)))

(defun is-small-cave (position)
  (not (equal position (string-upcase position))))

(defun num-paths (position visited graph)
  (format t "in: ~a~%" position)
  (if (equal position "end")
      (return-from num-paths 1))
  (let ((res 0))
    (dolist (neighbour (non-visited-neighbours position visited graph))
      (incf res (num-paths neighbour (concatenate 'list (list position) visited) graph)))
    (format t "out: ~a, ~a~%" position res)
    res))

(print (num-paths "start" NIL (build-adjacency-list (get-input-lines))))
