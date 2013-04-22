(in-package :kosips-asm)

(deftype dcpu-fixnum ()
  '(integer #x-ffff #xffff))

(deftype dcpu-infixnum ()
  '(integer -1 30))

(deftype dcpu-byte (&optional kind)
  (ecase kind
    (:signed '(signed-byte 16))
    (:unsigned '(unsigned-byte 16))
    (* '(or (dcpu-byte :signed) (dcpu-byte :unsigned)))))

(defvar *opcodes*
  '((basic .
     (("SET" . #x01)
      ("ADD" . #x02) ("SUB" . #x03) ("MUL" . #x04) ("MLI" . #x05)
      ("DIV" . #x06) ("DVI" . #x07) ("MOD" . #x08) ("MDI" . #x09)
      ("AND" . #x0a) ("BOR" . #x0b) ("XOR" . #x0c) ("SHR" . #x0d)
      ("ASR" . #x0e) ("SHL" . #x0f) ("IFB" . #x10) ("IFC" . #x11)
      ("IFE" . #x12) ("IFN" . #x13) ("IFG" . #x14) ("IFA" . #x15)
      ("IFL" . #x16) ("IFU" . #x17) ("ADX" . #x1a) ("SBX" . #x1b)
      ("STI" . #x1e) ("STD" . #x1f)))
    (special .
     (("JSR" . #x20)
      ("INT" . #x100) ("IAG" . #x120) ("IAS" . #x140) ("RFI" . #x160)
      ("IAQ" . #x180) ("HWN" . #x200) ("HWQ" . #x220) ("HWI" . #x240)))))

(defvar *registers*
  '((general .
     (("A" . 0) ("B" . 1) ("C" . 2) ("X" . 3) ("Y" . 4) ("Z" . 5)
      ("I" . 6) ("J" . 7)))
    (special .
     (("PC" . #x1b) ("SP" . #x1c) ("EX" . #x1d)))))

(defun byteify-fixnum (num)
  "Make fixnum into dcpu-byte"
  (declare (type dcpu-fixnum num))
  (the dcpu-byte (if (minusp num) (+ #x10000 num) num)))

;;;; Translator ;;;2;;;;;;;;;3;;;;;;;;;4;;;;;;;;;5;;;;;;;;;6;;;;;;;;;7

(defun translate-operator (op)
  (let ((basic (assocd 'basic *opcodes*))
        (special (assocd 'special *opcodes*)))
    (or (assocd op basic :test #'string-equal)
        (assocd op special :test #'string-equal))))

(defun translate-register (r)
  (let ((general (assocd 'general *registers*))
        (control (assocd 'control *registers*)))
    (or (assocd r general :test #'string-equal)
        (assocd r control :test #'string-equal))))

(defun translate-operand (spec)
  (match spec
    ((list :register r) (pair (translate-register r)))
    ((list :next num) (pair #x1f (the dcpu-byte num)))
    ((list (or :push :pop)) (pair #x18))
    ((list :peek) (pair #x19))
    ((list :pick num) (pair #x1a (the dcpu-byte num)))
    ((list :infix num) (pair (+ (the dcpu-infixnum num) 33)))
    ((list* :at place)
     (ematch place
       ((list (list :register r) offset)
        (pair (+ (translate-register r) #x10) (the dcpu-byte offset)))
       ((list (list :register r))
        (pair (+ (translate-register r) 8)))
       ((list (list :next num))
        (pair #x1e (the dcpu-byte num)))))))

(defun translate-instruction (operator operand-1 &optional operand-2)
  (let* ((opcode (translate-operator operator))
         (val-1 (translate-operand operand-1))
         (val-2 (translate-operand operand-2))
         (result (list opcode)))
    (format t "~S ~S ~S ~S" opcode val-1 val-2 result)
    (cond ((zerop (boole boole-and opcode #x1f))
           (assert (not operand-2) nil
                   "Too many operands for ~S" operator)
           (setf (ldb (byte 6 10) (car result)) (car val-1))
           (when (cdr val-1) (tpush (cdr val-1) result)))
          (t
           (assert (and operand-1 operand-2) nil
                   "Not enough operands for ~S" operator)
           (setf (ldb (byte 5 5) (car result)) (car val-1))
           (setf (ldb (byte 6 10) (car result)) (car val-2))
           (when (cdr val-2) (tpush (cdr val-2) result))
           (when (cdr val-1) (tpush (cdr val-1) result))))
    (values result)))

;;;; Preprocessor ;2;;;;;;;;;3;;;;;;;;;4;;;;;;;;;5;;;;;;;;;6;;;;;;;;;7

; Format for labels:
; defining form: (:definition (:label "name") addr)
; definition: ((:label "name") . addr)
; invocation: (:label "name")

(deftype definition-ident ()
  '(cons keyword (cons string)))

(deftype definition ()
  '(cons definition-ident t))

(deftype invocation ()
  '(cons keyword list))

(defstruct walking-context
  (pass 0               :type fixnum)
  (pass-differs nil     :type boolean)
  (byte-count 0         :type fixnum)
  (instruction-count 0  :type fixnum)
  (definitions (make-array '(32)
                           :element-type 'definition
                           :adjustable t
                           :fill-pointer 0)))

(defun increment-pass (context)
  (declare (walking-context context))
  (incf (walking-context-pass context))
  (setf (walking-context-pass-differs context) nil))

(defun pass-differs (context)
  (declare (walking-context context))
  (setf (walking-context-pass-differs context) t))

(defun increment-byte-count (context delta)
  (declare (walking-context context)
           (fixnum delta))
  (incf (walking-context-byte-count context) delta))

(defun increment-instruction-count (context)
  (declare (walking-context context))
  (incf (walking-context-instruction-count context)))

(defun definition-position (ident context)
  (declare (definition-ident ident)
           (walking-context context))
  (loop
     for def across (walking-context-definitions context)
     and i upfrom 0
     if (equal ident (car def)) do
       (return i)))

(defun push-definition (definition context)
  (declare (definition definition)
           (walking-context context))
  (let ((defs (walking-context-definitions context))
        (index (definition-position (car definition) context)))
    (if index
        (setf (elt defs index) (cdr definition))
        (vector-push-extend definition defs))))

(defun resolve-label (label context)
  (declare (invocation label)
           (walking-context context))
  (loop for d across (walking-context-definitions context)
     if (eql (caar d) :label)
     if (string-equal (cadar d) (cadr label)) do
       (return (cdr d))
     finally (return nil)))

(defun shift-labels (after-byte shift context)
  (declare (fixnum after-byte)
           (walking-context context))
  (loop for def across (walking-context-definitions context)
     if (eql (caar def) :label)
     if (> (cdr def) after-byte) do
       (incf (cdr def) shift)
     finally (return nil)))
