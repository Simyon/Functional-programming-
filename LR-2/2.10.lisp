;2.10
(defun terms3 (lst r-lst count) ; сумма 3-х слагаемых
                (+ (first r-lst) (second r-lst) (* 2 (nth count (reverse lst)))))                
                
(defun p-s-i (lst r-lst count) ; последовательность
                (if (null (third r-lst))
                                (+ (first r-lst) (second r-lst) (* 2 (second lst)))
                                (* (terms3 lst r-lst count) (p-s-i lst (rest r-lst) (+ count 1)))))
                
(defun product-sum3 (lst) ; точка входа
        (p-s-i lst lst 0))
