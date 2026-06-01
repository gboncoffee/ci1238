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
  `'(,indices ,sum))

(defmacro result-indices (result)
  `(car ,result))

(defmacro result-sum (result)
  `(car (cdr ,result)))

(defun get-best-auction (results current)
  (when (not (null results))
    (get-best-auction
     (cdr results)
     (if (is-better? (car results) current)
	 (car results)
	 (current)))))

(defmacro best-auction (results)
  `(get-best-auction (cdr ,results) (car ,results)))

(defun auction-branches (matrix indices sum)
  )

(defun auction-solver (matrix indices sum)
  (if (null matrix)
      (result indices sum)
      (best-auction (auction-branches matrix indices sum))))

(defun abey (matrix)
  (auction-solver matrix '() 0))

(defun main ()
  (when (member "-a" *args*)
    (set 'limit-function #'alternate-limit-function))
  (when (member "-f" *args*)
    (set 'cut-by-viability nil))
  (when (member "-o" *args*)
    (set 'cut-by-optimality nil))

  ;; Transpose the matrix as it's semantically better.
  (print (abey (transpose (read-matrix (read) (read))))))
