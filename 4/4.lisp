(with-open-file (stream "test.txt")
  (loop for line = (read-line stream nil)
     until (null line)
     do
       (print line)))

