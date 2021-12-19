(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun parse-point (line)
  (with-input-from-string (s
                           (substitute #\Space #\,
                                       (concatenate 'string "(" line ")")))
    (read s)))

(defun parse-scans (lines)
  (let ((scanners NIL) (i -1))
    (dolist (line lines)
      (cond
        ((search "---" line) (progn (setf scanners (append scanners (list NIL))) (incf i)))
        ((not (equal "" line)) (setf (elt scanners i) (append (elt scanners i) (list (parse-point line)))))))
    scanners))

(defparameter *orientations*
  (list (list (list #'first  #'second #'third)  (list  1  1  1))
        (list (list #'first  #'third  #'second) (list  1 -1  1))
        (list (list #'first  #'third  #'second) (list  1  1 -1))
        (list (list #'first  #'second #'third)  (list  1 -1 -1))
        (list (list #'second #'first  #'third)  (list -1  1  1))
        (list (list #'third  #'first  #'second) (list  1  1  1))
        (list (list #'second #'first  #'third)  (list  1  1 -1))
        (list (list #'third  #'first  #'second) (list -1  1 -1))
        (list (list #'third  #'second #'first)  (list -1  1  1))
        (list (list #'second #'third  #'first)  (list  1  1  1))
        (list (list #'third  #'second #'first)  (list  1 -1  1))
        (list (list #'second #'third  #'first)  (list -1 -1  1))
        (list (list #'first  #'second #'third)  (list -1  1 -1))
        (list (list #'first  #'third  #'second) (list -1  1  1))
        (list (list #'first  #'second #'third)  (list -1 -1  1))
        (list (list #'first  #'third  #'second) (list -1 -1 -1))
        (list (list #'second #'first  #'third)  (list  1 -1  1))
        (list (list #'third  #'first  #'second) (list  1 -1 -1))
        (list (list #'second #'first  #'third)  (list -1 -1 -1))
        (list (list #'third  #'first  #'second) (list -1 -1  1))
        (list (list #'third  #'second #'first)  (list  1  1 -1))
        (list (list #'second #'third  #'first)  (list  1 -1 -1))
        (list (list #'third  #'second #'first)  (list -1 -1 -1))
        (list (list #'second #'third  #'first)  (list -1  1 -1))))

(defun reorient-point (point orientation)
  (let ((reoriented (list NIL NIL NIL)))
    (dotimes (i 3)
      (setf (elt reoriented i) (* (funcall (elt (first orientation) i) point) (elt (second orientation) i))))
    reoriented))

(defun reorient-scan (scan orientation)
  (map 'list #'(lambda (point) (reorient-point point orientation)) scan))

(defun count-overlapping (scan1 point1 scan2 point2)
  (let ((delta1 (map 'list #'(lambda (p) (* -1 p)) point1))
        (delta2 (map 'list #'(lambda (p) (* -1 p)) point2)))
    (dolist (point scan1)
      (map-into point #'+ point delta1))
    (dolist (point scan2)
      (map-into point #'+ point delta2))
    (length (intersection scan1 scan2 :test #'tree-equal))))

(defun scans-overlap (scan1 scan2)
  (dotimes (i (length scan1))
    (dotimes (j (length scan2))
      (dolist (orientation *orientations*)
        (let* ((reoriented-scan2 (reorient-scan scan2 orientation))
               (num-overlapping (count-overlapping (copy-tree scan1) (elt scan1 i) reoriented-scan2 (elt reoriented-scan2 j))))
          (when (>= num-overlapping 12)
            ;(format t "overlap: ~a~%" num-overlapping)
            (return-from scans-overlap (list (map 'list #'- (elt scan1 i) (elt (reorient-scan scan2 orientation) j)) orientation)))))))
  NIL)

(defun remap (scan origin)
  (map 'list #'(lambda (p) (map 'list #'+ p origin)) scan))

(defparameter *scans* (parse-scans (get-input-lines)))

(defparameter *scanner-origins* (make-list 28))
(setf (elt *scanner-origins* 0) (list 0 0 0))

(defparameter *scanner-orientations* (make-list 28))
(setf (elt *scanner-orientations* 0) (first *orientations*))

;; Hard coded a couple routes to cover all scanners
(dolist (l (list (list 0 10 23 6)
                  (list 0 10 25 3 27 21 4 2 13 22)
                  (list 0 10 25 20 12 11 14 9 14 26 15 18 8 18 15 20 1 5 16 7 19)
                  (list 10 25 20 12 17)
                  (list 21 13 24)))
  (let ((curr (list 0 0 0)) (prev-scanner 0) (orientation (first *orientations*)))
    (dolist (i l)
      (let ((scan-res (scans-overlap (reorient-scan (copy-tree (elt *scans* prev-scanner)) orientation) (elt *scans* i))))
        (setf curr (map 'list #'+ curr (first scan-res)))
        (setf prev-scanner i)
        (setf orientation (second scan-res))
        (setf (elt *scanner-origins* i) curr)
        (setf (elt *scanner-orientations* i) orientation)))))

(let ((all-points NIL))
  (dotimes (i 28)
    (print (elt *origins* i))
    (setf all-points (append all-points (remap (reorient-scan (copy-tree (elt *scans* i)) (elt *scanner-orientations* i)) (elt *scanner-origins* i)))))
  (setf all-points (remove-duplicates all-points :test #'tree-equal))
  (print all-points)
  (print (length all-points)))

;; Used this to find overlapping scanners
;; (dotimes (i (length *scans*))
;;   (format t "scan ~a overlaps with: " i)
;;   (dotimes (j (length *scans*))
;;     (if (and (not (eql i j)) (scans-overlap (copy-tree (elt *scans* i)) (elt *scans* j)))
;;         (format t "~a, " j)))
;;   (format t "~%"))

;; scan 0 overlaps with: 10, 21, 22,
;; scan 1 overlaps with: 5, 20,
;; scan 2 overlaps with: 4, 13,
;; scan 3 overlaps with: 25, 27,
;; scan 4 overlaps with: 2, 21,
;; scan 5 overlaps with: 1, 16,
;; scan 6 overlaps with: 23,
;; scan 7 overlaps with: 16, 19, 25,
;; scan 8 overlaps with: 18,
;; scan 9 overlaps with: 14,
;; scan 10 overlaps with: 0, 23, 25, 27,
;; scan 11 overlaps with: 12, 14,
;; scan 12 overlaps with: 11, 17, 20, 26,
;; scan 13 overlaps with: 2, 21, 22, 24,
;; scan 14 overlaps with: 9, 11, 26,
;; scan 15 overlaps with: 18, 20, 26,
;; scan 16 overlaps with: 5, 7,
;; scan 17 overlaps with: 12,
;; scan 18 overlaps with: 8, 15,
;; scan 19 overlaps with: 7,
;; scan 20 overlaps with: 1, 12, 15, 25,
;; scan 21 overlaps with: 0, 4, 13, 27,
;; scan 22 overlaps with: 0, 13,
;; scan 23 overlaps with: 6, 10,
;; scan 24 overlaps with: 13,
;; scan 25 overlaps with: 3, 7, 10, 20,
;; scan 26 overlaps with: 12, 14, 15,
;; scan 27 overlaps with: 3, 10, 21,
