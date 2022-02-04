(ql:quickload :cl-ppcre)

;; part 1
(defun get-score (input-file)
  (let ((boards '())
	(numbers nil)
	(return-count 0)
	(current-board '()))
    (with-open-file (stream input-file)
      (loop for line = (read-line stream nil)
	 until (null line) do
	   (cond
	     ((string= line #\Return)
	      (incf return-count)
	      (if (and (> return-count 0) (not (null current-board)))
		  (progn (setf boards (append boards (list current-board)))
			 (setf current-board '()))))
	     ((= return-count 0)
	      (setf numbers
		    (map 'list
			 (lambda (n) (parse-integer n :junk-allowed t))
			 (cl-ppcre:split "," line))))
	     (t
	      (setf current-board (append
				   current-board
				   (list (map 'list (lambda (s) (parse-integer s))
					      (cl-ppcre:split "\\s+" (string-left-trim '(#\Space) line))))))))))
    (setf boards (append boards (list current-board)))
    (loop for i from 0 below (list-length boards) do
	 (let ((board-columns '()))
	   (loop for j from 0 below (list-length (nth i boards)) do
		(setf board-columns (append board-columns (list (map 'list (lambda (l) (nth j l)) (nth i boards))))))
	   (setf (nth i boards) (append (nth i boards) board-columns))))
    (let ((read-numbers '())
	  (winning-number nil)
	  (winning-board-index nil))
      (loop named find-winning-board for n in numbers do
	   (setf read-numbers (append read-numbers (list n)))
	   (loop for i from 0 below (list-length boards) do
		(loop for j from 0 below (list-length (nth i boards)) do
		     (let ((current-line (nth j (nth i boards))))
		       (if (subsetp current-line read-numbers)
			   (progn
			     (setf winning-number n)
			     (setf winning-board-index i)
			     (return-from find-winning-board)))))))
      (print read-numbers)
      (print winning-number)
      (print winning-board-index)
      (let ((sum-unmarked-numbers 0)
	    (winning-board (nth winning-board-index boards))
	    (current-line nil))
	(print winning-board)
	(loop for i from 0 below 5 do
	     (setf current-line (nth i winning-board))
	     (print current-line)
	     (loop for n in current-line do
		  (if (not (find n read-numbers)) (progn (print n) (incf sum-unmarked-numbers n)))))
	(* winning-number sum-unmarked-numbers)))))

(get-score "test.txt")
(get-score "input.txt")

;; part 2
(defun get-score-2 (input-file)
  (let ((boards '())
	(numbers nil)
	(return-count 0)
	(current-board '()))
    (with-open-file (stream input-file)
      (loop for line = (read-line stream nil)
	 until (null line) do
	   (cond
	     ((string= line #\Return)
	      (incf return-count)
	      (if (and (> return-count 0) (not (null current-board)))
		  (progn (setf boards (append boards (list current-board)))
			 (setf current-board '()))))
	     ((= return-count 0)
	      (setf numbers
		    (map 'list
			 (lambda (n) (parse-integer n :junk-allowed t))
			 (cl-ppcre:split "," line))))
	     (t
	      (setf current-board (append
				   current-board
				   (list (map 'list (lambda (s) (parse-integer s))
					      (cl-ppcre:split "\\s+" (string-left-trim '(#\Space) line))))))))))
    (setf boards (append boards (list current-board)))
    (loop for i from 0 below (list-length boards) do
	 (let ((board-columns '()))
	   (loop for j from 0 below (list-length (nth i boards)) do
		(setf board-columns (append board-columns (list (map 'list (lambda (l) (nth j l)) (nth i boards))))))
	   (setf (nth i boards) (append (nth i boards) board-columns))))
    (let ((read-numbers '())
	  (winning-number nil)
	  (winning-board nil))
      (loop while (> (list-length boards) 1) do
	   (setf boards (remove-nth (getf (find-winning-board boards numbers) :board-index) boards)))
      (let ((find-winning-board-response (find-winning-board boards numbers)))
	(setf
	 winning-number (getf find-winning-board-response :winning-number)
	 winning-board (car boards)
	 read-numbers (getf find-winning-board-response :read-numbers)))
      (format t "winning-number: ~a~%" winning-number)
      (let ((sum-unmarked-numbers 0)
	    (current-line nil))
	(loop for i from 0 below 5 do
	     (setf current-line (nth i winning-board))
	     (loop for n in current-line do
		  (if (not (find n read-numbers)) (incf sum-unmarked-numbers n))))
	(* winning-number sum-unmarked-numbers)))))

(defun find-winning-board (boards numbers)
  (let ((read-numbers '()))
    (loop named find-winning-board-index for n in numbers do
	 (setf read-numbers (append read-numbers (list n)))
	 (loop for board-index from 0 below (list-length boards) do
	      (let ((board (nth board-index boards)))
		(loop for line in board do
		     (if (subsetp line read-numbers)
			 (progn
			   (format t "winning board: ~a~%index: ~a~%winning-number: ~a~%" board board-index n)
			   (return-from find-winning-board-index
			     (list :board-index board-index
				   :winning-number n
				   :read-numbers read-numbers))))))))))

(defun remove-nth (n list)
  (declare
   (type (integer 0) n)
   (type list list))
  (if (or (zerop n) (null list))
      (cdr list)
      (cons (car list) (remove-nth (1- n) (cdr list)))))

(get-score-2 "test.txt")
(get-score-2 "input.txt")

