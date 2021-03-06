(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline)))

(defun russian-upper-case-p (char)
  (position char "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"))

(defun russian-char-downcase (char)
  (let ((i (russian-upper-case-p char)))
    (if i 
        (char "абвгдеёжзийклмнопрстуфхцчшщъыьэюя" i)
        (char-downcase char))))         ; латиница

(defun russian-string-downcase (string)
  ;; Преобразовать и латинские, и русские буквы строки в строчные
  (map 'string #'russian-char-downcase string))


(defun delete_end(list)                      
  (cond ((null (cdr list)) 
     nil)                        
    (t 
     (cons (car list) 
           (delete_end (cdr list))))))

  
  
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

(defun collect-letter-counts (word)
	(let ((ht (make-hash-table :test #'equal)))
		(loop for i upfrom 0 below (length word) do
			;; #'equal вставляет в конец слова с кодом #\\320, #\\321
			(when (char< (char word i) #\\320 ) (incf (gethash (char word i) ht 0))))
	ht))

(defun is-two-char-word (word)
	(let ((answer 0))
		(if (>= 2 (hash-table-count (collect-letter-counts word)))
			(setf answer 1))
	answer))

(defun remove-two-char-words(text)
	(loop for sentence in text
      collect (let ((predloz ()))
      	(dolist (word (word-list sentence))
      		(let ((string (string-right-trim ",.;:?!" (russian-string-downcase word))))
      				(when (< 0 (length string))
      					(when (= 0 (is-two-char-word (russian-string-downcase string)))
      						(setq predloz (append predloz (list word)))
      						(setq predloz (append predloz (list " ")))))))
      		(setq predloz (delete_end predloz)) ;;убрать последний пробел
      		(apply #'concatenate 'string predloz))))


