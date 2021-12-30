;; part 1
(defun get-power-consumption (input-file)
  (let ((counts '()))
    (with-open-file (stream input-file)
      (loop for line = (read-line stream nil)
	 until (null line)
	 do
	   (loop for index from 0 to (- (length line) 1)
	      do
		(let ((current-char (char line index)))
		  (if (= \0 char))))))))

(get-power-consumption "test.txt")

(map nil #'princ "bacon")

(apply '+ '(1 2 3))

(getf (list :0 0 :1 1) :4)

(eq (char "0" 0) (char "1" 0))
