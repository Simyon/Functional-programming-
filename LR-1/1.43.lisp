
(defun backspin-hours (h)        
; формула проще если час от 1 до 12
        (if (= h 0)     
                12
                h))
                
(defun norma-hour (h) (* 5 (backspin-hours h))) ; часы -> в минутные засечки


(defun parallel-hands-minutes (h m) 
; за каждые 12 минут часовая стрелка смещается на минуту
        (if (<= m (norma-hour h))     
                (multiple-value-bind (f s) (floor (* (- (norma-hour h) (/ (* 11 m) 12)) 12) 11) f)
                (multiple-value-bind (f s) (floor (* (- (+ 60 (norma-hour h)) (/ (* 11 m) 12)) 12) 11) f)))
