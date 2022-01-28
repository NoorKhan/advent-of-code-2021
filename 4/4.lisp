(ql:quickload "fset")
(ql:quickload :cl-ppcre)

(fset:equal? (fset:set 1 2 3 3) (fset:set 3 1 2))

(with-open-file (stream "test.txt")
  (loop for line = (read-line stream nil)
     until (null line)
     do
       (print line)))

(defun get-score (input-file)
  (let ((new-board nil)
	(boards nil)
	(numbers nil))
    (with-open-file (stream "test.txt")
     (loop for line = (read-line stream nil)
	until (null line)
	do
	  (print line)))))

(map 'list (lambda (n) (parse-integer n :junk-allowed t)) (cl-ppcre:split "," "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"))

(print (parse-integer "1#\Return" :junk-allowed t))



