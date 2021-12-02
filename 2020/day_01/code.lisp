(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-integer line))))

(defun find-k-sum-to-target (k target nums)
  (if (< k 2)
      (return-from find-k-sum-to-target
        (let ((found (find target nums)))
          (if found
              (make-array 1 :initial-element found :fill-pointer 1 :adjustable t)
              NIL)))
        (dolist (n nums)
          (let* ((sub-nums (remove n (copy-seq nums) :count 1))
                 (recurse (find-k-sum-to-target (1- k) (- target n) sub-nums)))
            (when recurse
              (vector-push-extend n recurse)
              (return-from find-k-sum-to-target recurse))
            NIL))))

(print (reduce #'* (find-k-sum-to-target 2 2020 (get-input-lines))))
(print (reduce #'* (find-k-sum-to-target 3 2020 (get-input-lines))))
