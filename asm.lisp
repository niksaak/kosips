(in-package :kosips-asm)

;;;; Vars

(defvar *basic-opcodes*
  '(("SET" . #x01) ("ADD" . #x02) ("SUB" . #x03)
    ("MUL" . #x04) ("MLI" . #x05) ("DIV" . #x06) ("DVI" . #x07)
    ("MOD" . #x08) ("MDI" . #x09) ("AND" . #x0a) ("BOR" . #x0b)
    ("XOR" . #x0c) ("SHR" . #X0d) ("ASR" . #X0e) ("SHL" . #x0f)
    ("IFB" . #x10) ("IFC" . #x11) ("IFE" . #x12) ("IFN" . #x13)
    ("IFG" . #x14) ("IFA" . #x15) ("IFL" . #x16) ("IFU" . #x17)
    ("ADX" . #x1a) ("SBX" . #x1b) ("STI" . #x1e) ("STD" . #x1f))
  "Basic DCPU-16 opcodes. (-----------*****)")

(defvar *special-opcodes*
  '(("JSR" . #x20)
    ("INT" . #x100) ("IAG" . #x120) ("IAS" . #x140) ("RFI" . #x160)
    ("IAQ" . #x180) ("HWN" . #x200) ("HWQ" . #x220) ("HWI" . #x240))
  "Special DCPU-16 opcodes. (------*****00000)")

(defvar *general-registers*
  '(("A" . 0) ("B" . 1) ("C" . 2) ("X" . 3) ("Y" . 4) ("Z" . 5)
    ("I" . 6) ("J" . 7))
  "General DCPU-16 registers.")

(defvar *control-registers*
  '(("SP" . #x1b) ("PC" . #x1c) ("EX" . #x1d))
  "Control DCPU-16 registers.")

;;;; Predicates

(defun basic-opcode (op)
  (if (typep op 'string)
      (assocd op *basic-opcodes* :test #'equal)))

(defun special-opcode (op)
  (if (typep op 'string)
      (assocd op *special-opcodes* :test #'equal)))

(defun general-register (r)
  (if (typep r 'string)
      (assocd r *general-registers* :test #'equal)))

(defun control-register (r)
  (if (typep r 'string)
      (assocd r *control-registers* :test #'equal)))

(defun push/pop (val)
  (case val
    ((push pop) #x18)))

(defun peek (val)
  (if (eql val 'peek)
      #xb19))

(defun pick (val)
  (if (typep val '(cons (eql pick) (cons dcpu-byte)))
      (list #x1a (cdr val))))

(defun label (val)
  (cond ((eq (elt val 0) #\:)
         (subseq val 1))
        ((eq (elt val (1- (length val))) #\:)
         (subseq val 0 (1- (length val))))
        (t nil)))

;;;; Types

;;;; instruction values:

(deftype dcpu-byte ()
  '(unsigned-byte 16))

(deftype dcpu-fixnum ()
  '(integer #x-ffff #xffff))

(deftype opcode (&optional kind)
  (case kind
    (basic '(satisfies basic-opcode))
    (special '(satisfies special-opcode))
    (* '(or (opcode basic) (opcode special)))))

(deftype register (&optional kind)
  (case kind
    (general '(satisfies general-register))
    (control '(satisfies control-register))
    (* '(or (register general) (register control)))))

(deftype push/pop ()
  '(satisfies push/pop))

(deftype peek ()
  '(satisfies peek))

(deftype pick ()
  '(satisfies pick))

(deftype label ()
  '(satisfies label))

;;;; clauses:

(defstruct (ref
             (:print-object print-ref))
  (at () :type (or (register general) dcpu-byte))
  (offset () :type (or dcpu-fixnum null)))

(deftype dcpu-operand ()
  '(or dcpu-byte register push/pop peek pick ref))

(defstruct (instruction
             (:print-object print-instruction))
  (op () :type (or opcode null))
  (val-1 () :type (or dcpu-operand null))
  (val-2 () :type (or dcpu-operand null)))

(defstruct (line
             (:print-object print-line))
  (label () :type (or string null))
  (instruction nil :type (or instruction null))
  (comment nil :type (or string null)))

(deftype clause (kind)
  "For future expansion"
  (case kind
    (line '(satisfies (lambda (thing) (typep thing 'line))))
    (* '(clause line))))

;;;; type-printers:

(defun print-ref (ref stream)
  (format stream "[~:[~A~@[~@Xh~]~;~Xh~]]"
          (numberp (ref-at ref))
          (ref-at ref)
          (ref-offset ref)))

(defun print-instruction (instr stream)
  (format stream "~A ~A, ~@[~A~]"
          (instruction-op instr)
          (instruction-val-1 instr)
          (instruction-val-2 instr)))

(defun print-line (line stream)
  (format stream "~@[~A ~]~@[~A ~]~@[;~A~]"
          (line-label line)
          (line-instruction line)
          (line-comment line)))

;;;; Functions

;;;; compiling:

(defun process-val (val &optional literate)
  (etypecase val
    (dcpu-byte
     (if (and literate (>= val -1) (<= val 30))
         (vector (+ #x21 val) nil)
         (vector #x1f val)))
    (register
     (vector (or (general-register val) (control-register val)) nil))
    (push/pop
     (vector #x18 nil))
    (peek
     (vector #x19 nil))
    (pick
     (vector #x1a (cadr val)))
    (ref
     (let ((at (ref-at val))
           (offset (ref-offset val)))
       (etypecase at
         ((register general)
          (if offset
              (vector (+ (general-register at) 16) offset)
              (vector (+ (general-register at) 8) nil)))
         (dcpu-byte
          (vector #x1e at)))))
    (null nil)))

(defun compile-instruction (instr)
  (flet ((preprocess (ins)
           (let ((op (instruction-op ins))
                 (val-1 (instruction-val-1 ins))
                 (val-2 (instruction-val-2 ins))
                 (pinstr (make-array 3)))
             (setf (elt pinstr 0)
                   (or #1=(basic-opcode op) #2=(special-opcode op)))
             (setf (elt pinstr 1) (process-val val-1 (special-opcode op)))
             (if #1#
                 (setf (elt pinstr 2) (process-val val-2 t)))
             pinstr)))
    (let* ((pinstr (preprocess instr))
           (result (list (elt pinstr 0)
                         (elt (elt pinstr 2) 1)
                         (elt (elt pinstr 1) 1))))
      (cond ((< (elt pinstr 0) #x20)
             (setf (ldb (byte 5 5) (car result)) (elt (elt pinstr 1) 0))
             (setf (ldb (byte 6 10) (car result)) (elt (elt pinstr 2) 0)))
            (t
             (setf (ldb (byte 6 10) (car result)) (elt (elt pinstr 1) 0))))
      result)))

;;;; reading:

(defun read-but-actually-skip-separators (stream)
  "it's marked with 'read', but actually it just skips separators \(^д^)/"
  (read-only-the-chars stream '(#\Tab #\Space #\,)))

(defun read-label (stream)
  (let ((startpos (file-position stream))
        (label (string-upcase (read-until-whitespace stream))))
    (if #1=(label label)
        #1#
        (progn (file-position stream startpos)
               nil))))

(defun read-op (stream)
  (let ((startpos (file-position stream))
        (op (string-upcase (read-until-whitespace stream))))
    (typecase op
      (opcode op)
      (otherwise (file-position stream startpos)
                 nil))))

(defun read-num (stream)
  (let* ((startpos (file-position stream))
         (numstring (read-until stream '(#\Tab #\Space #\Newline #\,)))
         (num (parse-prefixed-integer numstring)))
    (typecase num
      (dcpu-fixnum num)
      (integer (error "Integer is too big: ~S" num))
      (otherwise (file-position stream startpos)
                 nil))))

(defun read-register (stream)
  (let ((startpos (file-position stream))
        (string (string-upcase
                 (read-until stream '(#\Tab #\Space #\Newline #\,)))))
    (typecase string
      (register string)
      (otherwise (file-position stream startpos)
                 nil))))
