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

(let ((prb (make-instance 'probe :x-vel 6 :y-vel 9)))
  (print prb)
  (step-probe prb)
  (print prb))

;; All of the above is non-sense
;; Just need to do (geometric-sum (1- start-y)) ; from day_07
