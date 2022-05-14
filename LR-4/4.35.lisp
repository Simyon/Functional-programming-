
(defun replace-all-numbers (txt i index) 
        (if (= i index)
                (dotimes (j (position #\+ (nth i txt)))
                    (if (digit-char-p (aref (nth i txt) j))
                                (setf (aref (nth i txt) j) #\-)))
                (dotimes (j (length (nth i txt)))
                    (if (digit-char-p (aref (nth i txt) j))
                                (setf (aref (nth i txt) j) #\-))))
        txt)

(defun run-string (txt index) 
        (dotimes (i (+ index 1)) (replace-all-numbers txt i index))
        txt)

(defun where-first-plus (txt index) 
        (if index
                (run-string txt index)
                txt))

(defun first-plus (txt) 
        (let ((index))
        (dotimes (j (length txt))
                (let ((i (position #\+ (nth j txt))))
                        (when i (setq index j))
                        (when i (return))))
        index))

(defun plus2minusTxt (txt) (where-first-plus txt (first-plus txt)))

 
