;4.41
(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline)))

(defun word-list (string)
  ;; Разбить строки на слова, разделённые знаками whitespace
  ;; A la (split-seq-if #'whitespace-char-p string)
  (loop with len = (length string)
        for left = 0 then (1+ right)
        for right = (or (position-if #'whitespace-char-p string
                                     :start left)
                        len)
        unless (= right left)	; исключить пустые слова
          collect (subseq string left right)
        while (< right len)))
        
(defun max-digital-word-length (txt)
	;; возвращает длину слова
	;; возвращает само слово
	(let ((best-l 0)
	      (best-w))
	      (dolist (s txt)
	      	(dolist (w (word-list s))
	      		(when (and (every #'digit-char-p w) (>= (length w) best-l))
	      			(setf best-l (length w))
	      			(setf best-w w))))
	      (cons best-l best-w)))
