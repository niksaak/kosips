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
  (declare (type dcpu-fixnum num))
  (if (minusp num)
      (+ #x10000 num)
      num))

;;;; Translator

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
