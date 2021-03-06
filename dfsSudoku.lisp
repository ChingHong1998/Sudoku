;;; A depth-first search sudoku solver

;;; print the board
(defun print-board (b)
  (let ((n 0))
    (loop for x in b do
	  (cond ((or (= n 27) (= n 54))
		 (format t "~%---------+---------+---------~%"))

		((= (mod n 9) 0)
		 (format t "~%"))
		
		((= (mod n 3) 0)
		 (princ "|"))
		
		((= (mod n 9) 0)
		 (format t "~%")))
	  
	  (setf n (1+ n))
	  (format t " ~S " x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun board-has-duplicates-p (b)
  ;; T if there is a duplicate number in a row, column, or square
  
  (labels ((find-duplicates (x l) ; labels defines a local function named find-duplicates
			    (cond ((null x) '())	;if x is null, return an empty list
				  ((and (numberp (car x)) ; if first element type of x is number AND 
					(member (car x) l)) t) ; first element of x is member of l then return t
				  (t (find-duplicates (cdr x); else pass the rest of x in find-duplicates as 1st parameter 
						      (cons (car x) l))))) ;and combination of first element of x and l as second parameter
	   
           ;; check whether there are duplicates in a given row
	   (check-row (n b) ; local defined a function named check-row
		      (find-duplicates (subseq b (* n 9) (+ (* n 9) 9)) ; find-duplicates of row 
				       '()))

	   ;; check whether there are duplicates in a given column
	   (check-column (n b) ;local defined a function named check-row
			 (let ((column (mapcar (lambda (x) ; create a list called column
						 (nth (+ n (* x 9)) b)) ; that take n-th column of b
					       '(0 1 2 3 4 5 6 7 8))))
			   (find-duplicates column '()))) ; pass in the find-duplicates function

	   ;; check whether there are duplicates in a given square
	   (check-square (n b) ; if n = 1
			 (let* ((beg (* 2 (truncate (/ n 3)))) ; beg = 0
				(beg-1 (+ (* 3 n) (* beg 9))); beg-1 = 3
				(beg-2 (+ beg-1 9)) ; beg-2 = 12
				(beg-3 (+ beg-2 9)) ; beg-3 = 21
				(square (append (append (subseq b beg-1 ; (subseq b 3 6)
								(+ 3 beg-1))
							(subseq b beg-2 ; (subseq b 12 15)
								(+ 3 beg-2)))
						(subseq b beg-3 (+ 3 beg-3))))) ; (subseq b 21 24) square = {element 3-6 and element 12-15 and element 21-24}

			   (find-duplicates square '())))) ; (find-duplicates int that square)

	  ;; check all rows, columns, and squares
	  (loop for n from 0 to 8 do ;loop through all the number
		(print (check-row 8 b)) ; print row by row
		(cond ((or (check-row n b) (check-column n b) ; if row, colum 
			   (check-square n b)) ;and square dont have duplicate numbers
		       (return-from board-has-duplicates-p t)))))) ; return board-has-duplicates-p true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solve-sudoku (b)
  (with-open-stream ;performs a series of operations on stream, returns a value
   (*standard-output* (make-broadcast-stream)) ; a stream "make-broadcast-stream" is named as "*standard-output*" is opened
   (labels ((depth-first (b n) ;local define a function called depth-first
			 (cond ((= n 81) 
				(return-from solve-sudoku b)) ; Returns control and  values if n == 81
			       ((eq (nth n b) '-) ; if n-th of b equal to '-'
				(loop for x from 1 to 9 do ; range of x :1-9 do
				      (let ((newb ; let newb = something that
					     (append (subseq b 0 n) ; append (b[0-n]) 
						     (cons x (subseq b (1+ n) 81))))) ; with (x and b[(n+1) - 81]) which is rest of the array
					
					(cond ((not (board-has-duplicates-p newb)) ;while newb is not duplicate, 
					       (depth-first newb (1+ n))))))) ;depth-first search for the next number
			       (t (depth-first b (1+ n)))))) ; else depth-first search b with increment n
	   (depth-first b 0)))) ; return depth-first search (recursive call)
