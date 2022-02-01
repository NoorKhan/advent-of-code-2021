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
      (let ((sum-unmarked-numbers 0))
	(loop for line in (nth winning-board-index boards) do
	     (print line)
	     (loop for n in line do
		  (if (not (find n read-numbers)) (progn (print n) (incf sum-unmarked-numbers n)))))
	(* winning-number sum-unmarked-numbers)))))

(get-score "input.txt")
(get-score "input.txt")

(not nil)

(defparameter test 5)
(incf test 5)

(last '(1 2 3))

(subsetp '(1 2 3) '(4 5 6 3 2 1))

(append (list 1 4 5) '(1 2 3))

(string= #\Return #\Return)

(map 'list (lambda (n) (parse-integer n :junk-allowed t)) (cl-ppcre:split "," "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
"))

(print (parse-integer "1#\Return" :junk-allowed t))

(map 'list (lambda (s) (parse-integer s)) (cl-ppcre:split "\\s+" (string-left-trim '(#\Space) " 3 15  0  2 22
")))

(arrow-macros:-> " 3 15  0  2 22
" (lambda (s) (string-left-trim '(#\Space) s)) (lambda (s) (cl-ppcre:split "\\s+" s)) (map 'list (lambda (n) (parse-integer n :junk-allowed t))))

(string-trim 
      '(#\Space #\Newline #\Backspace #\Tab 
        #\Linefeed #\Page #\Return #\Rubout)
      "  A string   ")

