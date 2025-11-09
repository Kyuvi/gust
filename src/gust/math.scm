;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General Public license.
;;
;; Gust procedures and aliases that help with simple maths

(define-module (gust math)
  #:use-module ((gust core) #:select (def)))

;;;; shortcuts ;;;;


        ;;;; conversion ;;;;

(def float exact->inexact)
(def snap inexact->exact)

        ;;;; predicates ;;;;

(def num? number?)
(def float? inexact?)
(def -? negative?)
(def neg? negative?)
(def +? positive?)
(def pos? positive?)
(def 0? zero?)

(def (int? x)
  "(int? x)
   Return true (#t) if x is an exact integer."
  (and (integer? x) (exact? x)))

        ;;;; procedures ;;;;

(def rand random)

(def ceil ceiling)
(def trunc truncate)
(def inc 1+)
(def dec 1-)

(def quot quotient)
(def mod modulo)
(def rem remainder)

(def (cmprn x y)
  "(ncmp x y)
   Compare the numbers x and y returning -1, 0, 1 or +nan.0 depending on if x is
   less than, equal to, greater than y respectively.
   Otherwise returns 0 (e.g for +nan.0)."
  (cond ((= x y) 0)
        ((> x y) 1)
        ((< x y) -1)
        (else 0)))
