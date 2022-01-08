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
  (get-rate counts '>))

(defun get-epsilon-rate (counts)
  (get-rate counts '<))

(defun get-rate (counts f)
  (binary-to-decimal
   (reduce (lambda (x y) (concatenate 'string x (if (funcall f (getf y :0) (getf y :1)) "0" "1")))
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

(defun get-bit-count (binary-numbers index)
  (let ((bit-count (list :0 0 :1 0)))
    (loop for binary-number in binary-numbers
       do (if (char= #\0 (char binary-number index))
	      (incf (getf bit-count :0))
	      (incf (getf bit-count :1))))
    bit-count))

(defun get-oxygen-generator-rating (binary-numbers)
  (get-rating binary-numbers 0 #\1 '>))

(defun get-c02-scrubber-rating (binary-numbers)
  (get-rating binary-numbers 0 #\0 '<))

(defun get-rating (binary-numbers index default-char f)
  (if (> index (length (car binary-numbers))) (setf index 0))
  (if (= (list-length binary-numbers) 1)
      (binary-to-decimal (car binary-numbers))
      (let ((bit-count (get-bit-count binary-numbers index))
	    (key (if (char= #\0 default-char) :0 :1)))
	(if (or (funcall f (getf bit-count key) (getf bit-count (if (eq key :0) :1 :0)))
		(= (getf bit-count :0) (getf bit-count :1)))
	    (get-rating
	     (remove-if (lambda (binary-number) (char= (if (char= #\0 default-char) #\1 #\0) (char binary-number index))) binary-numbers) (1+ index) default-char f)
	    (get-rating
	     (remove-if (lambda (binary-number) (char= default-char (char binary-number index))) binary-numbers) (1+ index) default-char f)))))

(defvar *lines* (uiop:read-file-lines "test.txt"))
(* (get-oxygen-generator-rating *lines*) (get-c02-scrubber-rating *lines*))

(defvar *input* (uiop:read-file-lines "input.txt"))
(* (get-oxygen-generator-rating *input*) (get-c02-scrubber-rating *input*))

