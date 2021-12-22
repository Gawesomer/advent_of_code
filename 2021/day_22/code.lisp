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

(print (reboot-count (get-input-lines)))
