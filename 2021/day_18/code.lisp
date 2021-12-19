(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "test_input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun parse-snailnum (snailnum)
  (nsubstitute #\( #\[ snailnum)
  (nsubstitute #\) #\] snailnum)
  (nsubstitute #\Space #\, snailnum)
  (with-input-from-string (s snailnum)
    (read s)))

(defun add-snailnums (a b)
  (list a b))

(defparameter *explode-prev* NIL) ; closure
(defparameter *explode-next* NIL) ; int

(defun explode-step (snailnum depth unwind-stack)
  ;(format t "~a~%" snailnum)
  (if snailnum
      (if (numberp (car snailnum))
          (progn ;(format t "~a, ~a~%" depth snailnum)
                 (when *explode-next*
                   (incf (car snailnum) *explode-next*)
                   (funcall unwind-stack))
                 (if (and (>= depth 4) (numberp (cadr snailnum)) (not (cddr snailnum)))
                     (progn
                       ;(format t "explode: ~a~%" snailnum)
                       (if *explode-prev*
                           (funcall *explode-prev* (car snailnum)))
                       (setf *explode-next* (cadr snailnum))
                       (setf (car snailnum) 0)
                       (setf (cdr snailnum) NIL)))
                 (setf *explode-prev* #'(lambda (to-add) (incf (car snailnum) to-add)))
                 (explode-step (cdr snailnum) depth unwind-stack))
          (progn
            (explode-step (car snailnum) (1+ depth) unwind-stack)
            (explode-step (cdr snailnum) (if (not (cddr snailnum)) depth (1+ depth)) unwind-stack)))))

(defun explosion-cleanup (snailnum)
  (if (and snailnum (not (numberp snailnum)))
      (if (numberp (car snailnum))
          (explosion-cleanup (cdr snailnum))
          (progn
            (when (and (numberp (caar snailnum)) (not (cdar snailnum)))
              (setf (car snailnum) (caar snailnum)))
            (explosion-cleanup (car snailnum))
            (explosion-cleanup (cdr snailnum))))))

(defun split-step (snailnum unwind-stack)
  (if snailnum
      (if (numberp (car snailnum))
          (progn
            (if (>= (car snailnum) 10)
                (let ((left (floor (/ (car snailnum) 2))) (right (ceiling (/ (car snailnum) 2))))
                  (setf (car snailnum) (list left right))
                  (funcall unwind-stack)))
            (split-step (cdr snailnum) unwind-stack))
          (progn
            (split-step (car snailnum) unwind-stack)
            (split-step (cdr snailnum) unwind-stack)))))

;; (defun split-step (snailnum unwind-stack)
;;   (format t "split: ~a~%" snailnum)
;;   (when (and (numberp (car snailnum)) (numberp (cadr snailnum)) (not (cddr snailnum)))
;;     (if (>= (car snailnum) 10)
;;         (progn
;;           (let ((left (floor (/ (car snailnum) 2))) (right (ceiling (/ (car snailnum) 2))))
;;             (setf (car snailnum) (list left right))
;;             (funcall unwind-stack))))
;;     (if (>= (cadr snailnum) 10)
;;         (progn
;;           (let ((left (floor (/ (cadr snailnum) 2))) (right (ceiling (/ (cadr snailnum) 2))))
;;             (setf (cadr snailnum) (list left right))
;;             (funcall unwind-stack)))))
;;   (if (and (car snailnum) (not (numberp (car snailnum))))
;;       (split-step (car snailnum) unwind-stack))
;;   (if (and (cadr snailnum))
;;       (if (not (numberp (cadr snailnum)))
;;           (split-step (cadr snailnum) unwind-stack)
;;           (split-step (cdr snailnum) unwind-stack))))

(defun reduce-snailnum (snailnum)
  (do ((exploded T T) (splitted T T))
      (NIL NIL)
    (block explosion-step
      (setf *explode-prev* NIL)
      (setf *explode-next* NIL)
      (explode-step snailnum 0 #'(lambda () (return-from explosion-step)))
      (setf exploded NIL))
    (if exploded
        (progn
          (explosion-cleanup snailnum))
          ;(format t "explode:   ~a~%" snailnum))
        (progn
          (block splitting-step
            (split-step snailnum #'(lambda () (return-from splitting-step)))
            (setf splitted NIL))
          ;; (if splitted
          ;;     (format t "split:     ~a~%" snailnum))
          (if (and (not exploded) (not splitted))
              (return-from reduce-snailnum snailnum))))))

;(print (reduce-snailnum (parse-snailnum "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))

;; (let ((num (parse-snailnum "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")))
;;   (block reduction-step
;;     (explode-step num 0 #'(lambda () (return-from reduction-step))))
;;   (explosion-cleanup num)
;;   (print num))

;; (let ((num (parse-snailnum "[[[[0,7],4],[15,[0,13]]],[1,1]]")))
;;   (block reduction-step
;;     (split-step num #'(lambda () (return-from reduction-step))))
;;   (print num))

(defun sum-up (lines)
  (let ((snailnum (parse-snailnum (first lines))))
    ;(print (parse-snailnum (first lines)))
    (dotimes (i (1- (length lines)))
      (print snailnum)
      (setf snailnum (add-snailnums snailnum (parse-snailnum (elt lines (1+ i)))))
      (reduce-snailnum snailnum))
    snailnum))

(print (sum-up (get-input-lines)))
