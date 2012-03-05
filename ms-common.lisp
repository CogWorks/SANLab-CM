(defvar *log-io* *standard-output*)
(defparameter *block* t)
(defconstant +inf +1D++0)
(defconstant -inf -1D++0)

(defun flatten (l)
  (if (null l) nil
    (if (listp (car l)) (append (flatten (car l)) (flatten (cdr l)))
      (cons (car l) (flatten (cdr l))))))

(defun number-not-inf (x)
  (if (numberp x)
      (cond ((= x +inf) nil)
            ((= x -inf) nil)
            (t x))))

(defun newline (&optional (stream t))
  (format stream "~%"))

(defmacro say (&optional str &rest args)
  `(if *interactive* (progn (format *log-io* ,str ,@args) (if ,str (newline *log-io*)) (if *block* (read-line)))))

(defmethod mean ((sequence list)) (/ (reduce #'+ sequence) (length sequence)))

(defmacro square (x)
  (let ((val (gensym)))
    `(let ((,val ,x))
       (* ,val ,val))))

(defun standard-deviation (sequence)
  (let ((mean (mean sequence))
        (n (length sequence)))
    (sqrt (/ (reduce #'+ (map 'list #'(lambda (x) (square (- mean x))) sequence))
             (1- n)))))

(defun remove-pair (args key)
  (if (not args)
      nil
    (if (equal (first args) key)
        (cddr args)
      (cons (car args)
            (cons (cadr args)
                  (remove-pair (cddr args) key))))))

(defmethod alist-to-hash-table ((l list))
  (let ((ht (make-hash-table)))
    (do ((key (car l) (car l))
         (val (cadr l) (cadr l)))
        ((not l) ht)
      (setf (gethash key ht) val)
      (setf l (cddr l)))))

(defmethod as-list ((ht hash-table))
  (let (res)
    (maphash #'(lambda (k v) (declare (ignore k)) (push v res)) ht)
    res))

(defun copy-hash-table (table &key key test size
                                   rehash-size rehash-threshold)
  "Returns a copy of hash table TABLE, with the same keys and values
as the TABLE. The copy has the same properties as the original, unless
overridden by the keyword arguments.

Before each of the original values is set into the new hash-table, KEY
is invoked on the value. As KEY defaults to CL:IDENTITY, a shallow
copy is returned by default."
  (setf key (or key 'identity))
  (setf test (or test (hash-table-test table)))
  (setf size (or size (hash-table-size table)))
  (setf rehash-size (or rehash-size (hash-table-rehash-size table)))
  (setf rehash-threshold (or rehash-threshold (hash-table-rehash-threshold table)))
  (let ((copy (make-hash-table :test test :size size
                               :rehash-size rehash-size
                               :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v)
               (setf (gethash k copy) (funcall key v)))
             table)
    copy))

(defun max-func (items)
  (let ((max (first items)))
    (dolist (x (cdr items) max)
      (if (< max x) (setf max x))))
)