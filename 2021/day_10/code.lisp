(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun matching-bracket (bracket)
  (cond
    ((eql #\) bracket) #\()
    ((eql #\] bracket) #\[)
    ((eql #\> bracket) #\<)
    ((eql #\} bracket) #\{)
    ((eql #\( bracket) #\))
    ((eql #\[ bracket) #\])
    ((eql #\< bracket) #\>)
    ((eql #\{ bracket) #\})))

(defun is-corrupted (line)
  (do ((i 0 (1+ i)) (stack (make-array (length line) :fill-pointer 0)))
      ((eql i (length line)) NIL)
    (if (or
         (eql #\( (elt line i))
         (eql #\[ (elt line i))
         (eql #\{ (elt line i))
         (eql #\< (elt line i)))
        (vector-push (elt line i) stack)
        (if (eql (elt stack (1- (length stack))) (matching-bracket (elt line i)))
            (vector-pop stack)
            (return-from is-corrupted (elt line i))))))

(defun corrupted-score (lines)
  (let ((score 0))
    (dolist (line lines)
      (let ((corrupted (is-corrupted line)))
        (if corrupted
            (cond
              ((eql #\) corrupted) (incf score 3))
              ((eql #\] corrupted) (incf score 57))
              ((eql #\} corrupted) (incf score 1197))
              ((eql #\> corrupted) (incf score 25137))))))
    score))

(print (corrupted-score (get-input-lines)))

(defun complete-line (line)
  (do ((i (1- (length line)) (1- i))
       (stack (make-array (length line) :fill-pointer 0))
       (to-add (make-array (length line) :fill-pointer 0)))
      ((< i 0) to-add)
    (if (or
         (eql #\) (elt line i))
         (eql #\] (elt line i))
         (eql #\} (elt line i))
         (eql #\> (elt line i)))
        (vector-push (elt line i) stack)
        (if (and (> (length stack) 0)
                 (eql (elt stack (1- (length stack))) (matching-bracket (elt line i))))
            (vector-pop stack)
            (vector-push (matching-bracket (elt line i)) to-add)))))

(defun completion-score (completion)
  (let ((score 0))
    (dotimes (i (length completion))
      (setf score (* score 5))
      (cond
        ((eql #\) (elt completion i)) (incf score 1))
        ((eql #\] (elt completion i)) (incf score 2))
        ((eql #\} (elt completion i)) (incf score 3))
        ((eql #\> (elt completion i)) (incf score 4))))
  score))

(defun completion-scores (lines)
  (let ((scores NIL))
    (dolist (line lines)
      (push (completion-score (complete-line line)) scores))
    scores))

(print (sort (completion-scores (remove-if #'is-corrupted (get-input-lines))) #'<))
(print (elt (sort (completion-scores (remove-if #'is-corrupted (get-input-lines))) #'<) 25))
