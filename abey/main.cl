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

(defun get-best-auction (results current)
  (if (not (null results))
      (get-best-auction
       (cdr results)
       (if (result-is-better? (car results) current)
	   (car results)
	   current))
      current))

(defun concat-lists (a b)
  (if (null a)
      b
      (cons (car a) (concat-lists (cdr a) b))))

(defun best-auction (results)
  (get-best-auction (cdr results) (car results)))

(defun filter-viable (results)
  (if (null results)
      '()
      (if (viable (car results))
	  (cons (car results) (filter-viable (cdr results)))
	  (filter-viable (cdr results)))))

;; (defun insert-index (value idx results)
;;   (result (cons idx (result-indices results))
;; 	  (+ (result-sum results) value)))
;; We don't have to insert the index in the best result, we have to try
;; inserting it on all results and try without inserting it also.

(defun insert-indices-cutting (this-line idx result)
  ;; TODO.
  (insert-indices this-line idx result))

(defun insert-indices (this-line idx result)
  (if (null this-line)
      '()
      (cons (insert-index (car this-line) idx result)
	    (insert-indices (cdr this-line) (+ 1 idx) result))))

(defun auction-solver (matrix results)
  (get-best-auction
   (if (null matrix)
       results
       (let ((rest (auction-solver (cdr matrix) results)))
	 (cons rest
	       (concat-lists
		results
		(if cut-by-viability
		    (insert-indices-cutting (car matrix) 0 rest)
		    (filter-viable (insert-indices (car matrix) 0 rest)))))))
   (result '() 0)))

(defun abey (matrix)
  (auction-solver matrix `(,(result '() 0))))

(defun print-indices (indices &optional p)
  (let ((tp (or p 1)))
    (unless (null indices)
      (format t "~d ~d~%" tp (+ 1 (car indices)))
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

  ;; We transpose the matrix as it's semantically better.
  (let ((result (abey (transpose (read-matrix (read) (read))))))
    ;; We revert the indices as this list is constructed reverted.
    (let ((indices (reverse (result-indices result)))
	  (sum (result-sum result)))
      (print-indices indices)
      (print-sum sum))))

(defun self-reload ()
  (load "main.cl")
  (format t "~%")
  (main))
