(in-package :kosips-utils)

;;;; Optima patterns

(defpattern string-equal (string)
  `(guard it (string-equal it ,string)))

;;;; Predicating

(defun either (object &rest predicates)
  (if predicates
      (cond ((funcall (car predicates) object)
             t)
            ((cdr predicates)
             (either object (cdr predicates)))
            (t nil))))

;;;; List manipulation

(defun pair (&optional a b)
  (cons a b))

(defun assoca (item alist &key key (test #'eql))
  (car (assoc item alist :key key :test test)))

(defsetf assoca (item alist &key key (test #'eql)) (val)
  (rplaca (assoc item alist :key key :test test) val))

(defun assocd (item alist &key key (test #'eql))
  (cdr (assoc item alist :key key :test test)))

(defsetf assocd (item alist &key key (test #'eql)) (val)
  (rplacd (assoc item alist :key key :test test) val))

(defun rassoca (item alist &key key (test #'eql))
  (car (rassoc item alist :key key :test test)))

(defsetf rassoca (item alist &key key (test #'eql)) (val)
  (rplaca (rassoc item alist :key key :test test) val))

(defun rassocd (item alist &key key (test #'eql))
  (cdr (rassoc item alist :key key :test test)))

(defsetf rassocd (item alist &key key (test #'eql)) (val)
  (rplacd (rassoc item alist :key key :test test) val))

(defun tpush (item list)
  (nconc list (list item)))

;;;; Reading

(defun read-until (stream chars)
  "read from 'stream' until first occurance of any of 'chars'"
  (let ((array (make-array 64
                           :element-type 'character
                           :adjustable t
                           :fill-pointer 0)))
    (loop (if (not (typep (peek-char nil stream)
                          `(member ,@chars)))
              (vector-push-extend (read-char stream) array)
              (return)))
    (string array)))

(defun read-until-whitespace (stream)
  (read-until stream '(#\Tab #\Space #\Newline)))

(defun read-only-the-chars (stream chars)
  "read from 'stream' until first occurance of character not in 'chars'"
  (let ((array (make-array 64
                           :element-type 'character
                           :adjustable t
                           :fill-pointer 0)))
    (loop (if (typep (peek-char nil stream)
                     `(member ,@chars))
              (vector-push-extend (read-char stream) array)
              (return)))
    (string array)))

(defun parse-prefixed-integer (string)
  "parse integer with radix designated by its prefix,
which can be either 0r or #r, or suffix r, where r can be either
'x' 'o' or 'b', for radixes 16, 8 and 2 respectively."
  (flet ((radix (char)
           (cond
             ((or (char-equal char #\x)
                  (char-equal char #\h))
              16)
             ((char-equal char #\o)
              8)
             ((char-equal char #\b)
              2))))
    (let* ((afix-type
            (when (> (length string) 1)
              (cond ((and (or (char-equal (elt string 0) #\0)
                              (char-equal (elt string 0) #\#))
                          (radix (elt string 1)))
                     :prefix)
                    ((radix (elt string (1- (length string))))
                     :suffix))))
           (start (if (eql afix-type :prefix)
                      2
                      0))
           (end (if (eql afix-type :suffix)
                    (1- (length string))
                    (length string)))
           (radix (case afix-type
                    (:prefix (radix (elt string 1)))
                    (:suffix (radix (elt string end)))
                    (t 10))))
      (parse-integer string
                     :start start
                     :end end
                     :radix radix
                     :junk-allowed t))))
