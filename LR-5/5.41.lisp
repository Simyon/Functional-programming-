(defun square (x) (* x x))

(defclass cart ()                ; имя класса и надклассы
  ((x :initarg :x :reader cart-x)   ; дескриптор слота x
   (y :initarg :y :reader cart-y))) ; дескриптор слота y


(defmethod print-object ((c cart) stream)
    (format stream "[CART x:~d y:~d]"
        (cart-x c) (cart-y c)
    )
)

(defclass polar ()
  ((radius :initarg :radius :accessor radius) 	; длина >=0
   (angle  :initarg :angle  :accessor angle)))	; угол (-pi;pi]


(defmethod print-object ((p polar) stream)
  (format stream "[POLAR radius ~d angle ~d]"
          (radius p) (angle p)))


(defmethod radius ((c cart)) ; Получаю радиус
  (sqrt (+ (square (cart-x c))
           (square (cart-y c)))))


(defmethod angle ((c cart))
  (atan (cart-y c) (cart-x c)))	; atan2 в Си


(defmethod cart-x ((p polar)) ; Загоняю полярную координату в декартову
  (* (radius p) (cos (angle p))))


(defmethod cart-y ((p polar))
  (* (radius p) (sin (angle p))))

(defun check (A B C tolerance) ; Проверка разности
	(<= (abs (- (* (- (cart-x B) (cart-x A))
	   	   (- (cart-y C) (cart-y A)))
	       (* (- (cart-x C) (cart-x A))
	       (- (cart-y B) (cart-y A))))
	    ) tolerance
	))

(defun on-single-line (data)
	(if (>= 2 (length (first data))) 
		T
		(dotimes (i (length (first data)))
			(when (not (check (first (first data)) (second (first data)) (nth i (first data)) (second data)))(return-from on-single-line (not 1)))))
	T)

(defun on-single-line-p (vertices &optional (tolerance 0.001))  
	(on-single-line (list vertices tolerance)))
