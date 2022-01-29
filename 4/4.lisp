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
  (let ((new-board nil)
	(boards '())
	(numbers nil)
	(return-count 0)
	(current-board '()))
    (with-open-file (stream "test.txt")
     (loop for line = (read-line stream nil)
	until (null line)
	do
	  (cond
	    ((string= line #\Return)
	     (incf return-count)
	     (if (> return-count 0)
		 (progn (setf boards (append (list current-board) boards))
			(setf current-board '()))))
	    ((= return-count 0)
		 (print line)
	     (setf numbers
		   (map 'list
			(lambda (n) (parse-integer n :junk-allowed t))
			(cl-ppcre:split "," line))))
	    (t
	     (print line)
	     (setf current-board (append
	       (list (map 'list (lambda (s) (parse-integer s))
			  (cl-ppcre:split "\\s+" (string-left-trim '(#\Space) line))))
	       current-board))))))
    (setf boards (append (list current-board) boards))
    (print numbers)
    (print boards)
    (print (nth 5 boards))
    (print (list-length boards))))

(list-length '(1 2 3))

(get-score "test.txt")

(append (list 1) '(1 2 3))

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

