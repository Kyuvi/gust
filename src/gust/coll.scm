;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General Public license.
;;
;; Gust procedures and aliases that work on collections of objects

(define-module (gust coll)
  #:use-module (gust core))

        ;;;; lists ;;;;

(def lref list-ref)
(def ltail list-tail)
(def llen length)
(def mem member)
(def del delete)
(def del! delete!)

       ;;;;  vectors ;;;;

(def vref vector-ref)
(def mkvec make-vector)
(def vlen vector-length)

       ;;;;  strings ;;;;

(def sref string-ref)
(def mkstr make-string)
(def slen string-length)
(def apstr string-append)
(def concat string-append)

        ;;;; arrasys ;;;;

(def aref array-ref)
(cond-expand
 (guile-2 (def alen array-length))
 (guile (def (array-length a)
          "(alen a)
           Return the length of an array: its first dimension."
          (length (if (zero? (array-rank a))
                      (error "array-length: expecting a non-zero rank")
                      (array->list a))))

        (def alen array-length)))

        ;;;; collections ;;;;

;; TODO: add other vectors?
(defn get (dict k #:optional (not-found #f))
  "(get dict key #:optional not-found)
   Return the value associated with 'key' in dictionary 'dict' otherwise returns
   #f or not-found if provided.
   Works on association-lists, vectors, strings, hash-maps (guile-2+),
   bitvectors (guile-2+) and srfi-4 bytevectors.
   Vectors and strings take an integer as 'key'.
   Throws an error if 'dict' is not one of the dictionary types above."
  (let ((seq-fn (lambda (len-fn ref-fn)
                  (if (and (number? k) (>= (len-fn dict) k))
                      (ref-fn dict k)
                      not-found))))
    (cond ((list? dict) (let ((key-pair (assoc k dict)))
                          (if key-pair (cdr key-pair) not-found)))
          ((vector? dict) (seq-fn vector-length vector-ref))
          ((string? dict) (seq-fn string-length string-ref))
          ((cond-expand
            (guile-2
             (cond ((hash-table? dict) (hash-ref dict k not-found))
                   ((bitvector? dict) (seq-fn bitvector-length bitvector-ref))
                   (else #f)))))
          ((cond-expand
            (srfi-4
             (cond
              ((u8vector? dict) (seq-fn u8vector-length u8vector-ref))
              ((s8vector? dict) (seq-fn s8vector-length s8vector-ref))
              ((u16vector? dict) (seq-fn u16vector-length u16vector-ref))
              ((s16vector? dict) (seq-fn s16vector-length s16vector-ref))
              ((u32vector? dict) (seq-fn u32vector-length u32vector-ref))
              ((s32vector? dict) (seq-fn s32vector-length s32vector-ref))
              ((f32vector? dict) (seq-fn f32vector-length f32vector-ref))
              ((u64vector? dict) (seq-fn u64vector-length u64vector-ref))
              ((s64vector? dict) (seq-fn s64vector-length s64vector-ref))
              ((f64vector? dict) (seq-fn f64vector-length f64vector-ref))
              (else #f)))))
          (else (error "get: Incompatable object," dict)))))

       ;;;;  sequences ;;;;

(defn vec (obj)
  "(vec obj)
   Coerce 'obj' to a vector if possible, otherwise throws an error
   (guile-2+: hash-tables are converted to a vector of vectors
   (i.e. an array))."
  (cond ((vector? obj) obj)
        ((list? obj) (list->vector obj))
        ((string? obj) (list->vector (string->list obj)))
        ((cond-expand
          (guile-2
           (cond ((hash-table? obj)
                  (list->vector (hash-map->list
                                 (fn (x y) (list->vector (list x y))) obj)))
                 (else #f)))))
        (else (error "vec: Incompatable object," obj))))

(defn enlist (obj)
  "(enlist obj)
   Convert 'obj' to a list if possible, otherwise throws an error
   (guile-2+: hash-tables are converted to alists.)."
  (cond ((list? obj) obj)
        ((vector? obj) (vector->list obj))
        ((string? obj) (string->list obj))
        ((array? obj) (array->list obj))
        ((cond-expand
          (srfi-4
           (cond ((u8vector? obj) (u8vector->list obj))
                 ((s8vector? obj) (s8vector->list obj))
                 ((u16vector? obj) (u16vector->list obj))
                 ((s16vector? obj) (s16vector->list obj))
                 ((u32vector? obj) (u32vector->list obj))
                 ((s32vector? obj) (s32vector->list obj))
                 ((f32vector? obj) (f32vector->list obj))
                 ((u64vector? obj) (u64vector->list obj))
                 ((s64vector? obj) (s64vector->list obj))
                 ((f64vector? obj) (f64vector->list obj))
                 (else #f)))))
        ((cond-expand
          (srfi-14
           (cond ((char-set? obj) (char-set->list obj))
                 (else #f)))))
        ((cond-expand
          (guile-2
           (cond ((bitvector? obj) (bitvector->list obj))
                 ((hash-table? obj) (hash-map->list (fn (x y) (cons x y)) obj))
                 (else #f)))))
       (else (error "enlist: Incompatable object," obj))))

(defn nth (obj n)
  "(nth obj n)
   Return the value at index 'n' in object 'obj', otherwise throws an error."
  (cond ((list? obj) (list-ref obj n))
        ((vector? obj) (vector-ref obj n))
        ((string? obj) (string-ref obj n))
        ((cond-expand
          (srfi-4
           (cond ((u8vector? obj) (u8vector-ref obj n))
                 ((s8vector? obj) (s8vector-ref obj n))
                 ((u16vector? obj) (u16vector-ref obj n))
                 ((s16vector? obj) (s16vector-ref obj n))
                 ((u32vector? obj) (u32vector-ref obj n))
                 ((s32vector? obj) (s32vector-ref obj n))
                 ((f32vector? obj) (f32vector-ref obj n))
                 ((u64vector? obj) (u64vector-ref obj n))
                 ((s64vector? obj) (s64vector-ref obj n))
                 ((f64vector? obj) (f64vector-ref obj n))
                 (else #f)))))
        ((cond-expand
          (guile-2
           (cond ((bitvector? obj) (bitvector-ref obj n))
                 ((struct? obj) (struct-ref obj n))
                 (else #f)))))
        (else (error "nth: incompatable object," obj))))

(defn len (obj)
  "(len obj)
   Return the number of items in the object 'obj' or in the
   string representation of object, otherwise throws an error."
  (cond ((list? obj) (length obj))
        ((vector? obj) (vector-length obj))
        ((string? obj) (string-length obj))
        ((array? obj) (array-length obj))
        ((symbol? obj) (string-length (symbol->string obj)))
        ((number? obj) (string-length (number->string obj)))
        ((cond-expand
          (srfi-4
           (cond ((u8vector? obj) (u8vector-length obj))
                 ((s8vector? obj) (s8vector-length obj))
                 ((u16vector? obj) (u16vector-length obj))
                 ((s16vector? obj) (s16vector-length obj))
                 ((u32vector? obj) (u32vector-length obj))
                 ((s32vector? obj) (s32vector-length obj))
                 ((f32vector? obj) (f32vector-length obj))
                 ((u64vector? obj) (u64vector-length obj))
                 ((s64vector? obj) (s64vector-length obj))
                 ((f64vector? obj) (f64vector-length obj))
                 (else #f)))))
        ((cond-expand
          (guile-2
           (cond ((bitvector? obj) (bitvector-length obj))
                 ((hash-table? obj) (hash-count (const #t) obj))
                 (else #f)))))
        (else (error "len: incompatable object," obj))))
