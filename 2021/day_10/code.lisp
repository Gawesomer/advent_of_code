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
    ((eql #\} bracket) #\{)))

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
