(defclass probe ()
  ((x
    :initform 0
    :accessor x)
   (y
    :initform 0
    :accessor y)
   (x-vel
    :initarg :x-vel
    :accessor x-vel)
   (y-vel
    :initarg :y-vel
    :accessor y-vel)))

(defmethod print-object ((prb probe) stream)
  (print-unreadable-object (prb stream :type NIL)
    (format stream "pos: (~a, ~a), vel: (~a, ~a)" (x prb) (y prb) (x-vel prb) (y-vel prb))))

(defmethod step-probe ((prb probe))
  (with-accessors ((x x) (y y) (x-vel x-vel) (y-vel y-vel)) prb
    (incf x x-vel)
    (incf y y-vel)
    (cond
      ((> x-vel 0) (decf x-vel))
      ((< x-vel 0) (incf x-vel)))
    (decf y-vel)))

(defmethod hitp ((prb probe) start-x end-x start-y end-y)
  (with-accessors ((x x) (y y)) prb
    (and (>= x start-x)
         (<= x end-x)
         (>= y start-y)
         (<= y end-y))))

(defmethod approachingp ((prb probe) start-x end-x start-y end-y)
  (and (<= (x prb) end-x)
       (>= (y prb) start-y)))

(defmethod will-hitp ((prb probe) start-x end-x start-y end-y)
  (do ()
      ((not (approachingp prb start-x end-x start-y end-y)) NIL)
    (if (hitp prb start-x end-x start-y end-y)
        (return-from will-hitp T))
    (step-probe prb)))

(defun count-hits (start-x end-x start-y end-y max-y)
  (let ((num-hits 0))
    (do ((x 1 (1+ x)))
        ((> x end-x) NIL)
      (do ((y start-y (1+ y)))
          ((> y max-y) NIL)
        (if (will-hitp (make-instance 'probe :x-vel x :y-vel y) start-x end-x start-y end-y)
            (incf num-hits))))
    num-hits))

;; All of the above is non-sense
;; For part 1 just need to do (geometric-sum (1- start-y)) ; from day_07

;(print (count-hits 20 30 -10 -5 45))
(print (count-hits 128 160 -142 -88 10011)) ; Couldn't be bothered to speed this up...
