(defun max-on-line (line)
  (if (null line)
      0
      (max (car line) (max-on-line (cdr line)))))

(defun max-on-matrix (matrix)
  (if (null matrix)
      0
      (max (max-on-line (car matrix)) (max-on-matrix (cdr matrix)))))

(defun sum-max-per-line (matrix)
  (if (null matrix)
      0
      (+ (max-on-line (car matrix)) (sum-max-per-line (cdr matrix)))))

(defun default-limit-function (result remaining)
  (+ (result-sum result)
     (sum-max-per-line remaining)))

(defun alternate-limit-function (result remaining)
  (+ (result-sum result)
     (* (max-on-matrix remaining) (length remaining))))

(defparameter limit-function #'default-limit-function)
(defparameter cut-by-viability t)
(defparameter cut-by-optimality t)
(defparameter number-of-branches 0)

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

(defparameter best-result-yet (result '() 0))

(defun best-auction (results)
  (if (null results)
      (result '() 0)
      (let ((this-result (car results))
	    (next-result (best-auction (cdr results))))
	;; Check if viable because if we're not cutting not viable branches
	;; they would appear here. It's a workaround and not pretty at all.
	(when (and (viable this-result)
		   (result-is-better? this-result best-result-yet))
	  (set 'best-result-yet this-result))
	(if (result-is-better? this-result next-result)
	    this-result
	    next-result))))

(defun branch-can-be-optimal? (result remaining)
  (> (funcall limit-function result remaining) (result-sum best-result-yet)))

(defun auction-solver (value idx remaining r)
  (let ((new-result (add-to-result r idx value)))
    (when (or (not cut-by-viability) (viable new-result))
      (if (or
	   (not cut-by-optimality)
	   (branch-can-be-optimal? new-result remaining))
	  (progn
	    (set 'number-of-branches (+ number-of-branches 1))
	    (best-auction
	     (run-abey (car remaining) 1 (cdr remaining) new-result)))
	  new-result))))

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
	     (rest
	      (run-abey (cdr this-line) (+ 1 idx) remaining r)))
	 (if (null this-branch)
	     rest
	     (cons this-branch rest))))))

(defun abey (matrix)
  (best-auction (run-abey (car matrix) 1 (cdr matrix) (result '() 0))))

(defun print-indices (indices &optional p)
  (let ((tp (or p 1)))
    (unless (null indices)
      (format t "~d ~d~%" tp (car indices))
      (print-indices (cdr indices) (+ tp 1)))))

(defun print-sum (sum)
  (format t "~d~%" sum))

(defun print-number-of-branches ()
  (format *error-output* "nós: ~d~%" number-of-branches))

(defun print-time (time)
  (format *error-output* "runtime: ~dus~%" time))

(defun main ()
  (set 'number-of-branches 0)
  (set 'best-result-yet (result '() 0))
  (when (member "-a" *args* :test #'string=)
    (set 'limit-function #'alternate-limit-function))
  (when (member "-f" *args* :test #'string=)
    (set 'cut-by-viability nil))
  (when (member "-o" *args* :test #'string=)
    (set 'cut-by-optimality nil))

  (let ((begin (get-internal-run-time)))
    (let ((result (abey (read-matrix (read) (read)))))
      (let ((end (get-internal-run-time)))
	;; The indices list is created reversed, so we reverse it again.
	(print-indices (reverse (result-indices result)))
	(print-sum (result-sum result))
	(print-number-of-branches)
	(print-time (- end begin))))))

(defun self-reload ()
  (load "main.cl")
  (format t "~%")
  (main))
