(defun default-limit-function ())

(defun alternate-limit-function ())

(defparameter limit-function #'default-limit-function)
(defparameter cut-by-viability t)
(defparameter cut-by-optimality t)

(defun read-row (n cur)
  (if (= cur 0)
      '()
      (cons (read) (read-row n (- cur 1)))))

(defun read-rows (p c cur)
  (if (= cur 0)
      '()
      (cons (read-row c c)
	    (read-rows p c (- cur 1)))))

(defun read-matrix (p c)
  (read-rows p c p))

(defun transpose (matrix)
  (if (null (car matrix))
      '()
      (cons (mapcar #'car matrix) (transpose (mapcar #'cdr matrix)))))

(defmacro result (indices sum)
  '`(,indices ,sum))

(defmacro result-indices (result)
  `(car ,result))

(defmacro result-sum (result)
  `(car (cdr ,result)))

(defun check-viability (indices)
  (unless (member (car indices) (cdr indices))
    (check-viability (cdr indices))))

(defmacro viable (result)
  `(check-viability (result-indices ,result)))

(defun result-is-better? (a b)
  (> (result-sum a) (result-sum b)))

(defun get-best-auction (results current)
  (if (not (null results))
      (get-best-auction
       (cdr results)
       (if (result-is-better? (car results) current)
	   (car results)
	   current))
      current))

(defun make-auction-branch (this-line remaining-matrix idx indices sum)
  (if (not cut-by-viability)
      (auction-solver
       remaining-matrix
       (cons idx indices)
       (+ sum
	  (car this-line)))
      (if (member idx indices)
	  (result indices sum)
	  (auction-solver
	   remaining-matrix
	   (cons idx indices)
	   (+ sum
	      (car this-line))))))

(defun make-auction-branches (this-line remaining-matrix idx indices sum)
  (if (null this-line)
      '()
      (cons (make-auction-branch this-line remaining-matrix idx indices sum)
	    (make-auction-branches (cdr this-line) remaining-matrix
				   (+ idx 1) indices sum))))

(defmacro auction-branches (matrix indices sum)
  `(make-auction-branches (car ,matrix) (cdr ,matrix) 0 ,indices ,sum))

(defun best-auction (results)
  (get-best-auction (cdr results) (car results)))

(defun filter-viable (results)
  (if (null results)
      '()
      (if (viable (car results))
	  (cons (car results) (filter-viable (cdr results)))
	  (filter-viable (cdr results)))))

(defun auction-solver (matrix indices sum)
  (if (null matrix)
      (result indices sum)
      (best-auction
       (if cut-by-viability
	   (auction-branches matrix indices sum)
	   (filter-viable (auction-branches matrix indices sum))))))

(defun abey (matrix)
  (auction-solver matrix '() 0))

(defun print-indices (indices &optional p)
  (let ((tp (or p 1)))
    (unless (null indices)
      (format t "~d ~d~%" tp (car indices))
      (print-indices (cdr indices) (+ tp 1)))))

(defun print-sum (sum)
  (format t "~d~%" sum))

(defun main ()
  (when (member "-a" *args*)
    (set 'limit-function #'alternate-limit-function))
  (when (member "-f" *args*)
    (set 'cut-by-viability nil))
  (when (member "-o" *args*)
    (set 'cut-by-optimality nil))

  ;; We transpose the matrix as it's semantically better. We store the
  (let ((result (abey (transpose (read-matrix (read) (read))))))
    ;; We revert the indices as this list is constructed reverted.
    (let ((indices (reverse (result-indices result)))
	  (sum (result-sum result)))
      (print-indices indices)
      (print-sum sum))))
