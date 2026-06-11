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

(defun result (indices sum)
  `(,indices ,sum))

(defun result-indices (result)
  (car result))

(defun result-sum (result)
  (car (cdr result)))

(defun check-viability (indices)
  (if (null indices)
      t
      (if (member (car indices) (cdr indices))
	  nil
	  (check-viability (cdr indices)))))

(defun viable (result)
  (check-viability (result-indices result)))

(defun result-is-better? (a b)
  (> (result-sum a) (result-sum b)))

(defun add-to-result (r idx value)
  (result (cons idx (result-indices r))
	  (+ value (result-sum r))))

(defun get-best-auction (results current)
  (if (not (null results))
      (get-best-auction
       (cdr results)
       (if (result-is-better? (car results) current)
	   (car results)
	   current))
      current))

(defun best-auction (results)
  (get-best-auction (cdr results) (car results)))

(defun auction-solver (value idx remaining r)
  (let ((new-result (add-to-result r idx value)))
    (best-auction (run-abey (car remaining) 1 (cdr remaining) new-result))))

(defun filter-viable-impl (results)
  (if (null results)
      '()
      (let ((r (car results))
	    (rest (filter-viable-impl (cdr results))))
	(if (viable r)
	    (cons r rest)
	    rest))))

(defun filter-viable (results)
  (if cut-by-viability
      results
      (filter-viable-impl results)))

(defun run-abey (this-line idx remaining r)
  (if (null this-line)
      `(,r)
      (filter-viable
       (let ((this-branch
	      (auction-solver (car this-line) idx remaining r))
	     (rest (run-abey (cdr this-line) (+ 1 idx) remaining r)))
	 (if (or (not cut-by-viability) (viable this-branch))
	     (cons this-branch rest)
	     rest)))))

(defun abey (matrix)
  (best-auction (run-abey (car matrix) 1 (cdr matrix) (result '() 0))))

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

  (let ((result (abey (read-matrix (read) (read)))))
    ;; The indices list is created reversed, so we reverse it again.
    (print-indices (reverse (result-indices result)))
    (print-sum (result-sum result))))

(defun self-reload ()
  (load "main.cl")
  (format t "~%")
  (main))
