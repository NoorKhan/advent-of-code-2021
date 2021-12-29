(ql:quickload :cl-ppcre)

;; part 1
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
	     (cond ((equalp "forward" direction) (setf horizontal-position (+ horizontal-position distance)))
		   ((equalp "down" direction) (setf depth (+ depth distance)))
		   ((equalp "up" direction) (setf depth (- depth distance)))))))
    (* horizontal-position depth)))

(calculate-position "input.txt")

;; part 2
(defun calculate-correct-position (input-file)
  (let ((horizontal-position 0)
	(depth 0)
	(aim 0))
    (with-open-file (stream input-file)
      (loop for line = (read-line stream nil)
	 until (null line)
	 do
	   (let* ((input (cl-ppcre:split "\\s+" line))
		  (direction (nth 0 input))
		  (units (parse-integer (nth 1 input))))
	     (cond ((equalp "forward" direction) (setf horizontal-position (+ horizontal-position units)
						       depth (+ depth (* aim units))))
		   ((equalp "down" direction) (setf aim (+ aim units)))
		   ((equalp "up" direction) (setf aim (- aim units)))))))
    (* horizontal-position depth)))

(calculate-correct-position "test.txt")
(calculate-correct-position "input.txt")

