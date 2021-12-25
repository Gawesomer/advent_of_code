(defun get-input-lines ()
  "Reads lines from file named input.txt"
  (with-open-file (in "input.txt")
    (loop for line = (read-line in nil nil)
          while line
          collect (cl-utilities:split-sequence #\Space line))))

(defclass ALU ()
  ((code
    :initarg :code
    :accessor code)
   (pc ; Program Counter
    :initform 0
    :accessor pc)
   (input
    :initarg :input
    :accessor input)
   (ip ; Input Pointer (talk about poor naming...)
    :initform 0
    :accessor ip)
   (registers
    :initform (list 0 0 0 0)
    :accessor registers)))

(defmethod print-object ((alu ALU) stream)
  (print-unreadable-object (alu stream :type NIL)
    (format stream "~a" (registers alu))))

(defun label->register (label)
  (cond
    ((equal label "w") 0)
    ((equal label "x") 1)
    ((equal label "y") 2)
    ((equal label "z") 3)))

(defun mnemo->op (mnemo)
  (cond
    ((equal mnemo "add") #'+)
    ((equal mnemo "mul") #'*)
    ((equal mnemo "div") #'truncate)
    ((equal mnemo "mod") #'mod)
    ((equal mnemo "eql") #'(lambda (a b) (if (eql a b) 1 0)))))

(defun mnemo->symbol (mnemo)
  (cond
    ((equal mnemo "add") #\+)
    ((equal mnemo "mul") #\*)
    ((equal mnemo "div") #\\)
    ((equal mnemo "mod") #\%)
    ((equal mnemo "eql") #\=)))

(defmethod run-instruction ((alu ALU))
  (let ((instruction (elt (code alu) (pc alu))))
    (incf (pc alu))
    (if (equal (first instruction) "inp")
        (progn (setf (elt (registers alu) (label->register (second instruction)))
                     (parse-integer (elt (input alu) (ip alu))))
               (incf (ip alu)))
        (let ((operand (parse-integer (third instruction) :junk-allowed T)))
          (if (not operand)
              (setf operand (elt (registers alu) (label->register (third instruction)))))
          (setf (elt (registers alu) (label->register (second instruction)))
                (funcall (mnemo->op (first instruction))
                         (elt (registers alu) (label->register (second instruction))) operand))))))

(defmethod run ((alu ALU))
  (do ()
      ((eql (pc alu) (length (code alu))) NIL)
    (run-instruction alu))
  (print alu))

(defun int->input (i)
  (map 'list #'(lambda (c) (make-array 1 :initial-element c :element-type 'character)) (format NIL "~a" i)))

(run (make-instance 'ALU :code (get-input-lines) :input (int->input 53999995829399)))
(run (make-instance 'ALU :code (get-input-lines) :input (int->input 11721151118175)))

;Brute forcing didn't work
;11111111926942
;99999872598575
;; (do ((i 99999872598575))
;;     (NIL NIL)
;;   (let ((input (int->input i)))
;;     (if (eql (count "0" input :test #'equal) 0)
;;         (let ((alu (make-instance 'ALU :code (get-input-lines) :input input)))
;;           (run alu)
;;           (when (eql (elt (registers alu) 3) 0)
;;             (print i)
;;             (return))))
;;     (decf i)))
