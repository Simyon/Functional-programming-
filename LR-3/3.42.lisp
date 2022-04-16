
(defun calc-value (n i j)
        (if (evenp j)
                (- (* n (+ 1 j)) (- n i 1))
                (- (* n (+ 1 j)) i)))

(defun create-matrix (n)
        (let ((matrix (make-array (list n n))))
        (dotimes (i n)
                 (dotimes (j n)
                         (setf (aref matrix i j) (calc-value n i j))))
        matrix))
        
(defun print-matrix (matrix &optional (chars 3) stream)
  (let ((*print-right-margin* (+ 6 (* (1+ chars) 
                                      (array-dimension matrix 1)))))
    (pprint matrix stream)
    (values)))


(defun matrix-tl-br (n) (create-matrix n))



(print-matrix (matrix-tl-br 1))
(print-matrix (matrix-tl-br 4))
(print-matrix (matrix-tl-br 9))
