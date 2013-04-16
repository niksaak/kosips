(in-package :kosips-asm-2)

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

(defvar *registers* #("A" "B" "C" "X" "Y" "Z" "I" "J"))

;;;; translator:

(defun byteify-fixnum (num)
  (if (minusp num)
      (+ #x10000 num)
      num))

(defun retpair (a &optional b)
  (cons a b))

(defun op (opcode)
  (let ((basic (assocd 'basic *opcodes*))
        (special (assocd 'special *opcodes*)))
    (or (assocd opcode basic :test #'string-equal)
        (assocd opcode special :test #'string-equal)
        (error "bad opcode: ~a" opcode))))

(defun register (r)
  (ematch r
    ((string-equal "PC") #x1b)
    ((string-equal "SP") #x1c)
    ((string-equal "EX") #x1d)
    (_ (position r *registers* :test #'string-equal))))

(defun val (spec)
  (ematch spec
    ((list :register r) (retpair (register r)))
    ((list :next num) (retpair #x1f (the dcpu-byte num)))
    ((list (or :push :pop)) (retpair #x18))
    ((list :peek) (retpair #x19))
    ((list :pick num) (retpair #x1a (byteify-fixnum num)))
    ((list :infix num) (retpair (+ (the dcpu-infixnum num) 33)))
    ((list :at (list :register r) offset) (retpair (+ (register r) #x10)
                                                   (byteify-fixnum offset)))
    ((list :at place)
     (ematch place
       ((list :register r) (retpair (+ (register r) 8)))
       ((list :next num) (retpair #x1e (the dcpu-byte num)))))))

(defun instruction (op value-1 &optional value-2)
  (let ((opcode (op op))
        (val1 (val value-1))
        (val2 (if value-2 (val value-2)))
        (result (list)))
    (push opcode result)
    (cond ((zerop (boole boole-and opcode #x1f))
           (setf (ldb (byte 6 10) (car result)) (car val1))
           (when (cdr val1) (tpush (cdr val1) result))
           result)
          (:else
           (setf (ldb (byte 5 5) (car result)) (car val1))
           (setf (ldb (byte 6 10) (car result)) (car val2))
           (when (cdr val2) (tpush (cdr val2) result))
           (when (cdr val1) (tpush (cdr val1) result))
           result))))

(defmacro instruct (operator operand-1 &optional operand-2)
  `(instruction ',operator ',operand-1 ',operand-2))

;;;; reader:

(defun parse-next (string)
  (let ((num (parse-prefixed-integer string)))
    (when num `(:next ,num))))

(defun parse-infix (string)
  (let ((num (parse-prefixed-integer string)))
    (when (typep num 'dcpu-infixnum) `(:infix ,num))))

(defun parse-num (string)
  (or (parse-infix string) (parse-next string)))

(defun parse-register (string)
  (when (or (find string *registers* :test #'string-equal)
            (string-equal string "PC")
            (string-equal string "SP")
            (string-equal string "EX"))
    `(:register ,string)))

(defun parse-subinstruction (string)
  (match string
    ((or (string-equal "PUSH")
         (string-equal "[--SP]"))
     '(:push))
    ((or (string-equal "POP")
         (string-equal "[SP++]"))
     '(:pop))
    ((or (string-equal "PEEK")
         (string-equal "[SP]"))
     '(:peek))
    (_ nil)))

(defun parse-pick (string)
  (register-groups-bind (pick num)
      ("^(pick|PICK)\\s+(\\S*)$" string)
    (when pick
      (let ((int (parse-prefixed-integer num)))
        (if int (list :pick int))))))

(defun parse-reference (string)
  (when (and (char= (elt string 0) #\[)
             (char= (elt string (- (length string) 2))))
    (let ((str (subseq string 1 (- (length string) 1))))
      (register-groups-bind (a b)
          ("^\\s*([^\\s+]+)(?:\\s*\\+\\s*(\\S+))?\\s*$"
           str)
        (cond
          (b (cond
               ((parse-register a)
                (list :at (list :register a) (parse-prefixed-integer b)))
               ((parse-register b)
                (list :at (list :register b) (parse-prefixed-integer a)))))
          (a (cond
               ((parse-register a)
                (list :at (list :register a)))
               ((parse-prefixed-integer a)
                (list :at (list :next (parse-prefixed-integer a)))))))))))

(defun parse-val (string &optional long)
  (when string
    (or (if long
            (parse-num string)
            (parse-next string))
        (parse-register string)
        (parse-subinstruction string)
        (parse-pick string)
        (parse-reference string))))

(defun asm (string)
  (register-groups-bind (op val1 val2)
      ("^(\\S+)\\s+((?:pick|PICK)\\s+[^\\s,]+|[^\\s,]+)(?:(?:\\s+|\\s*,\\s*)((?:pick|PICK)\\s+\\S+|\\S+))?$"
       string)
    (instruction op (parse-val val1) (parse-val val2 t))))
