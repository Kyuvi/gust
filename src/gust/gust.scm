;; Copyright © 2025 Kyuvi..
;; This file is published under version 3 of the GNU General Public license.
;;
;; Main Gust module re-exporting procedures from
;; core
;; coll (collections)
;; obj (objects)
;; os gos (Gust operating system)
;; math
;; io (input/output)

(define-module (gust gust)
  #:use-module (gust core)
  #:use-module (gust coll)
  #:use-module (gust obj)
  #:use-module (gust os gos)
  #:use-module (gust math)
  #:use-module (gust io)
  ;; #:use-module (gust sx)

  #:re-export (
                ;; from core.scm
                   ;;  procedures
                def- def
                fn prc
                prc? fn?
                defn defn-
                use defd?
                ;; NOTE: does not re-export lambda-keys or replace-optn!

                ;; from obj.scm
                true? false? void?
                num str sym keyword name

                ;; from coll.scm
                    ;;;  lists
                lref ltail llen mem del del!
                    ;;;  vectors
                vref mkvec vlen
                    ;;;  strings
                sref mkstr slen apstr concat
                    ;;; arrays
                aref alen
                    ;; multiple collections
                get vec enlist nth len

                ;; from os/gos.scm
                sys sh pwd cd
                os-sep
                decode-time 

                ;; from math.scm
                rand
                snap float
                int? float? -? neg? +? pos? 0?
                ceil inc dec
                quot mod rem cmprn

                ;; from io.scm
                nln wr wrn pr prn
                fmt prfm printf))

(read-set! keywords 'prefix) ;; use simple keywords, i.e. :key rather than #:key

;; guile-1.x catchup
(cond-expand
 (guile-2
  ;; syntax functions are part of guile-2+, but not guile-1
  (use-modules (gust sx))

  (re-export sx defsx sxrule sxcase defalias defrule))

 (guile
  (re-export
   ;; from core.scm
   lambda* let-optional let-optional* let-keywords let-keywords*)))
