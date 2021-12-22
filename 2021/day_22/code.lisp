(defun parse-line (line)
  (let* ((parsed (list NIL NIL NIL NIL))
         (space-split (cl-utilities:split-sequence #\Space line))
         (coordinates-split (cl-utilities:split-sequence #\, (second space-split))))
    (setf (elt parsed 0) (equal (first space-split) "on"))
    (dotimes (i 3)
      (let ((start-end-split (cl-utilities:split-sequence #\. (subseq (elt coordinates-split i) 2))))
        (setf (elt parsed (1+ i)) (list (parse-integer (first start-end-split)) (parse-integer (third start-end-split))))))
    parsed))

(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-line line))))

(defun range->coordinates (x-range y-range z-range)
  (let ((coordinates NIL))
    (do ((x (max (first x-range) -50) (1+ x)))
        ((> x (min (second x-range) 50)) coordinates)
      (do ((y (max (first y-range) -50) (1+ y)))
          ((> y (min (second y-range) 50)) NIL)
        (do ((z (max (first z-range) -50) (1+ z)))
            ((> z (min (second z-range) 50)) NIL)
          (push (coordinate->int (list x y z)) coordinates))))))

(defun coordinate->int (coordinate)
  (if (or (or (< (first coordinate) -50) (> (first coordinate) 50))
          (or (< (second coordinate) -50) (> (second coordinate) 50))
          (or (< (third coordinate) -50) (> (third coordinate) 50)))
      (return-from coordinate->int NIL)
      (+ (* (+ (first coordinate) 50) 100 100) (* (+ (second coordinate) 50) 100) (+ (third coordinate) 50))))

(defun reboot-count (reboot-steps)
  (let ((cubes (hash-set:make-hash-set)))
    (dolist (reboot-step reboot-steps)
      (print reboot-step)
      (if (first reboot-step)
          (setf cubes (hash-set:hs-union cubes (hash-set:list-to-hs
                                                (remove NIL (range->coordinates (second reboot-step) (third reboot-step) (fourth reboot-step))))))
          (setf cubes (hash-set:hs-difference cubes (hash-set:list-to-hs
                                                     (remove NIL (range->coordinates (second reboot-step) (third reboot-step) (fourth reboot-step))))))))
    (hash-set:hs-count cubes)))

;(print (reboot-count (get-input-lines)))

(defun count-range (range)
  (*
   (- (second (first range)) (first (first range)) -1)
   (- (second (second range)) (first (second range)) -1)
   (- (second (third range)) (first (third range)) -1)))

(defun line-intersection (range1 range2)
  (cond
    ((and
      (>= (first range1) (first range2))
      (<= (first range1) (second range2)))
     (list (first range1) (min (second range1) (second range2))))
    ((and
      (>= (first range2) (first range1))
      (<= (first range2) (second range1)))
     (list (first range2) (min (second range1) (second range2))))))

(defun range-intersection (range1 range2)
  (let ((x-overlap (line-intersection (first range1) (first range2)))
        (y-overlap (line-intersection (second range1) (second range2)))
        (z-overlap (line-intersection (third range1) (third range2))))
    (if (and x-overlap y-overlap z-overlap)
        (list x-overlap y-overlap z-overlap))))

(defun empty-range (range)
  (or
   (< (second (first range)) (first (first range)))
   (< (second (second range)) (first (second range)))
   (< (second (third range)) (first (third range)))))

(defun range-difference (range to-remove)
  (if (not to-remove)
      (return-from range-difference (list range)))
  (if (tree-equal range to-remove)
      (return-from range-difference NIL))
  (let ((res NIL))
    (push (list ; Bottom
           (first range)
           (list (first (second range)) (1- (first (second to-remove))))
           (third range))
          res)
    (push (list ; Top
           (first range)
           (list (1+ (second (second to-remove))) (second (second range)))
           (third range))
          res)
    (push (list ; Large side
           (first range)
           (second to-remove)
           (list (first (third range)) (1- (first (third to-remove)))))
          res)
    (push (list ; Large side
           (first range)
           (second to-remove)
           (list (1+ (second (third to-remove))) (second (third range))))
          res)
    (push (list ; Small side
           (list (first (first range)) (1- (first (first to-remove))))
           (second to-remove)
           (third to-remove))
          res)
    (push (list ; Small side
           (list (1+ (second (first to-remove))) (second (first range)))
           (second to-remove)
           (third to-remove))
          res)
    (remove-if #'empty-range res)))

(defun add-range (to-add ranges)
  (let ((res NIL))
    (dolist (existing ranges)
      (setf res (append res (range-difference existing (range-intersection existing to-add)))))
    (append res (list to-add))))

(defun remove-range (to-remove ranges)
  (let ((res NIL))
    (dolist (existing ranges)
      (setf res (append res (range-difference existing (range-intersection existing to-remove)))))
    res))

(defun sum-ranges (ranges)
  (reduce #'+ (map 'list #'count-range ranges)))

(defun reboot-count-full (reboot-steps)
  (let ((ranges NIL))
    (dolist (reboot-step reboot-steps)
      (if (first reboot-step)
          (setf ranges (add-range (list (second reboot-step) (third reboot-step) (fourth reboot-step)) ranges))
          (setf ranges (remove-range (list (second reboot-step) (third reboot-step) (fourth reboot-step)) ranges))))
    (sum-ranges (remove-duplicates ranges :test #'tree-equal))))

(print (reboot-count-full (get-input-lines)))
