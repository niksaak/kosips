(defpackage kosips-utils
  (:use cl optima)
  (:export either
           assoca assocd rassoca rassocd tpush
           read-only-the-chars read-until read-until-whitespace whitespacep
           parse-prefixed-integer))

(defpackage kosips-asm
  (:use cl kosips-utils))

(defpackage kosips-asm-2
  (:use cl kosips-utils optima cl-ppcre))
