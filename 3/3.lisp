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
		  (if (null (nth index counts))
		      (setf counts (append counts (list (list :0 0 :1 0)))))
		  (if (eq #\0 current-char)
		      (incf (getf (nth index counts) :0))
		      (incf (getf (nth index counts) :1)))))))
    counts))

(get-power-consumption "test.txt")

(map nil #'princ "bacon")

(apply '+ '(1 2 3))

(defvar *list* (list :0 0 :1 1))


(incf (getf *list* :0))

(defvar *my-list* (list 1 2 3))

(append *my-list* (list 4))
*my-list*
(append (list 1 2 3) (list 4))

(nth 1 '())

*list*

(eq (char "0" 0) (char "1" 0))
