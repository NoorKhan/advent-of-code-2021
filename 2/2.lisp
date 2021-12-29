(ql:quickload :cl-ppcre)

(defun calculate-position (input-file)
  (let ((horizontal-position 0)
	(depth 0))
    (with-open-file (stream input-file)
      (loop for line = (read-line stream nil)
	 until (null line)
	 do
	   (let* ((input (cl-ppcre:split "\\s+" line))
		  (direction (nth 0 input))
		  (distance (parse-integer (nth 1 input))))
	     (print (concatenate 'string direction (write-to-string distance)))
	     (cond ((equalp "forward" direction) (setf horizontal-position (+ horizontal-position distance)))
		   ((equalp "down" direction) (setf depth (+ depth distance)))
		   ((equalp "up" direction) (setf depth (- depth distance)))))))
    (* horizontal-position depth)))

(calculate-position "input.txt")

(write-to-string 5)

(equalp "forward" "forward")

