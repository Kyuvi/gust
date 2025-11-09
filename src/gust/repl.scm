;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General Public license.
;;
;; REPL funnctions that speed up REPL use,
;; but not usually neccessary for deployed code.

(define-module (gust repl)
  #:use-module (gust core)
  #:use-module ((gust obj) #:select (str))
  #:use-module ((gust os gos) #:select (sh))
  #:use-module ((ice-9 documentation) #:select (object-documentation))
  #:use-module (ice-9 pretty-print)
  #:export (doc)
  #:re-export (pretty-print object-documentation))


(cond-expand
 (guile-2
  (use-modules ((system repl common) #:select (repl-default-option-set!)))
  ;; stop guile from entering the debugger on-error
  ;; can be set to 'debug 'report or 'backtrace with the
  ;; ",o[ption]" repl command.
  (repl-default-option-set! 'on-error 'report))

 (guile #f))

        ;;;; properties ;;;;

(define-macro (doc obj)
  "(doc obj)
    Display the documentation string associated with 'obj'."
  `(begin
     (display (object-documentation (module-ref (current-module) (quote ,obj))))
     (newline)))

(def src procedure-source)

        ;;;; system ;;;;

(def qn primitive-exit)
(def xx exit)

(defn pp (#:optional obj (port (current-output-port)))
  "(pp #:optional obj port)
   Pretty print 'obj' to 'port'.
   obj defaults to last returned object or throws an error if no history).
   port defaults to current output port. "
  (let ((object (if obj
                    obj
                    (let* ((history (resolve-module '(value-history)))
                           (hist-vals
                            (delete! #f ;; remove non-numerical vals in guile-1
                                     (module-map
                                      (fn (k v)
                                          (string->number
                                           (substring (symbol->string k) 1)))
                                      history))))
                      (if (null? hist-vals)
                          (error "pp: No history yet")
                          (module-ref history
                                      (string->symbol
                                       (str "$" (car (sort hist-vals >))))))))))
    (pretty-print object port)))

(defn ls (#:optional (paths-str "")) (sh "ls " paths-str))
(defn lsa (#:optional (paths-str "")) (sh "ls -A " paths-str))

(def (vi fl-path) (sh  "vim " fl-path))

(def (ed fl)
  (let ((sys-ed (getenv "EDITOR")))
    (sh (if sys-ed sys-ed "vim") " " fl)))

(defn rd (#:optional n)
  "(rd #:optional n)
   Change direcotry to the directory 'n' levels above the current directory.
   'n' defaults to 1."
   (if n
       (chdir (apply string-append (make-list n "../")))
       (chdir "..")))

        ;;;; modules ;;;;

(defn module-loader (base)
  "(module-loader base)
   Returns a function whose arguments are modules from the module collection 'base'
   and loads said modules.
   The function will only works for single folder modules, i.e. (base module),
   and does not reload modules."
  (lambda (. modz)
    (let mdl-loop ((mod-i modz))
      (if (pair? mod-i)
          (begin
            (module-use! (current-module)
                         (resolve-interface (list base (car mod-i))))
            (mdl-loop (cdr mod-i)))))))

(def (iced . modz)
  "(iced #:rest modz)
   Uses the ice-9 modules represented by 'modz'."
  (apply (module-loader 'ice-9) modz))

(def (srfi . modz)
  "(srfi #:rest modz)
   Uses the srfi modules represented by 'modz', which can be symbols and
   numbers."
  (let ((srfi-modz (map (lambda (x) (if (number? x)
                                        (string->symbol (str "srfi-" x))
                                        x))
                        modz)))
    (apply (module-loader 'srfi) srfi-modz)))

(defn module-bindings (#:optional mod public list-prep)
  "(module-bindings #:optional mod public list-prep)
   Print (or return an alist of) the variables defined in (or exported from) the
   module 'mod'.
   if 'public' is true, return/print all exported variables, otherwise only
   defined variables.
   if 'list-pred' is true ,return an alist, otherwise Print to current-output-port.
   'mod' defaults to (current-module), 'public' and 'list-pred' default to #f."
  (let* ((module-obj (if public
                         (resolve-interface
                          (if mod mod (module-name (current-module))))
                         (if mod (resolve-module mod) (current-module))))
         (pp-var (lambda (x) (if (variable? x) (variable-ref x) x))))
    (if list-prep
        (module-map (lambda (k v) (cons k (pp-var v))) module-obj)
        (module-for-each (lambda (k v) (format #t "~23A ~A\n" k (pp-var v)))
                   module-obj))))

(def bindz module-bindings)

(defn module-exported (#:optional mod list-pred)
  "(module-publics mod #:optional list-prep)
   Print (or return an alist of) the exported variables defined in module 'mod'.
   if 'list-pred' is true return an alist otherwise Print to current-output-port.
   mod defaults to (current-module)."
  (module-bindings mod #t list-pred))

;; NOTE: very likely to be removed in future
(def ns-publics module-exported)

(def pubz module-exported)

        ;;;; macros ;;;;

(def mx macroexpand)

(cond-expand
 (guile-2
  (def (mx1 arg)
    (format #t "~%NOTE: macroexpand-1 is not available after ~:@
                guile-2, due to issues with syntax-case. ~@
                The repl command ',expand' (,exp) or macroexpand ~:@
                can be used instead.~%")))
 (guile (def mx1 macroexpand-1)))
