(ql:quickload :cl-ppcre)

;; part 1
(defun get-power-consumption (input-file)
  (let ((counts '()))
    (with-open-file (stream input-file)
      (loop for line = (read-line stream nil)
	 until (null line)
	 do
	   (loop for index from 0 below (length (car (cl-ppcre:split "\\s+" line)))
	      do
		(let ((current-char (char line index)))
		  (if (null (nth index counts))
		      (setf counts (append counts (list (list :0 0 :1 0)))))
		  (if (char= #\0 current-char)
		      (incf (getf (nth index counts) :0))
		      (incf (getf (nth index counts) :1)))))))
    (* (get-gamma-rate counts) (get-epsilon-rate counts))))

(defun get-gamma-rate (counts)
  (binary-to-decimal
   (reduce (lambda (x y) (concatenate 'string x (if (> (getf y :0) (getf y :1)) "0" "1")))
	   counts :initial-value "")))

(defun get-epsilon-rate (counts)
  (binary-to-decimal
   (reduce (lambda (x y) (concatenate 'string x (if (< (getf y :0) (getf y :1)) "0" "1")))
	   counts :initial-value "")))

(defun binary-to-decimal (binary-number)
  (let ((decimal-number 0)
	(exponent (1- (length binary-number))))
    (loop for index from 0 to (- (length binary-number) 1)
       do (setf decimal-number (+ decimal-number
				  (* (digit-char-p (char binary-number index)) (expt 2 exponent))))
	 (decf exponent))
    decimal-number))

(get-power-consumption "test.txt")
(get-power-consumption "input.txt")

;; part 2
