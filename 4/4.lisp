(ql:quickload :fset)
(ql:quickload :cl-ppcre)
(ql:quickload :arrow-macros)

(fset:equal? (fset:set 1 2 3 3) (fset:set 3 1 2))

(with-open-file (stream "test.txt")
  (loop for line = (read-line stream nil)
     until (null line)
     do
       (print line)))

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
      (print boards)
      (print "beginning loops")
      (loop named find-winning-board for n in numbers do
	   (setf read-numbers (append read-numbers (list n)))
	   (loop named loop-boards for i from 0 below (list-length boards) do
		(let ((board (nth i boards)))
		  (loop for line in board do
		      (if (subsetp line read-numbers)
			  (if (= (list-length boards) 1)
			      (progn (setf winning-board board)
				     (setf winning-number n)
				     (return-from find-winning-board))
			      (progn (setf boards (remove-nth i boards))
				     (return-from loop-boards))))))))
      (print boards)
      (print winning-number)
      (let ((sum-unmarked-numbers 0)
	    (current-line nil))
	(print winning-board)
	(loop for i from 0 below 5 do
	     (setf current-line (nth i winning-board))
	     (print current-line)
	     (loop for n in current-line do
		  (if (not (find n read-numbers)) (progn (print n) (incf sum-unmarked-numbers n)))))
	(* winning-number sum-unmarked-numbers)))))

(get-score-2 "test.txt")

(defparameter test '(1 2 3))

(setf test '(1 2 3))

(remove-if #'equal '(1 2) '((1 2) (1 3) (4) (1 2 3)))

(delete 3 test)
test
