;; Copyright © 2025 Kyuvi.
;; This file is published under version 3 of the GNU General Public license.
;;
;; Gust procdures and aliases for interacting with files on some systems

(define-module (gust os files)
  #:use-module (gust gust)
  #:autoload (srfi srfi-1) (lset-intersection))

(cond-expand (guile-2 #f)
             (guile (use-modules ((srfi srfi-13) #:select (string-join)))))

(defn build-path (. args)
  "(build-path #:rest args)
   Join all arguments together seperated by the file-name-seperator-string."
  (string-join args os-sep))

(defn symlink? (path)
  "(symlink? path)
   Test if 'path' is a symlink."
  (eq? (stat:type (lstat path)) 'symlink))

(defn real-directory? (path)
  "(real-directory path)
   Test if 'path' is a directory that is not a symlink."
  (and (not (symlink? path)) (file-is-directory? path)))

(defn directory-files (#:optional (path (getcwd)) (select? #t) (sort? string<?))
  "(directory-files #:optional path  select? sort?)
   List files in directory 'path' (defaults to current working directory).
   Select files using 'select?' procedure.
   'select?' can be ...
                  #f - list all files (including '.' and '..') .
                  #:hidden - include hidden files (excluding '.' and '..')
                  a procedure - select returned files using procedure.
                  #t - list all visible files.
   select? defaults to #t.
   'sort?' can be '#f' in which case, do not sort, otherwise it should be a
   predicate used for comparing string values.
   'sort'? defaults to 'string<?'."
  (let  ((path-stream (catch 'system-error
                             (lambda () (opendir path))
                             (const #f)))
         (select-fn (cond ((false? select?) identity)
                          ((eq? select? #:hidden)
                           (lambda (x) (not (member x (list ".." ".")))))
                              ;; (or (string=? x "..") (string=? x "."))
                          ((procedure? select?) select?)
                          (#:else (lambda (x) (not (string-prefix? "." x)))))))
    (if path-stream
        (let file-loop ((file (readdir path-stream))
                        (file-list '()))
          (if (eof-object? file)
              (begin
                (closedir path-stream)
                (if sort? (sort file-list sort?) file-list))
              (file-loop (readdir path-stream)
                         (if (select-fn file)
                             (cons file file-list)
                             file-list))))
        #f)))

(def gls directory-files)

(defn copy-directory (dir dest #:optional (recursive #f))
  "(copy-directory dir dest #:optional recursive)
   Copy directory 'dir' to 'dest', if 'recursive' is true copy recursively.
   Recursive defaults to #f. Does not follow symlinks."
  (cond ((real-directory? dir)
         (let* ((contents (directory-files dir #:hidden))
                (empty (null? contents)))
           (if empty
               (mkdir dest)
               (if recursive
                   (begin
                     (if (not (file-exists? dest)) (mkdir dest))
                     (let cpy-loop ((files contents))
                       (if (pair? files)
                           (let* ((next-file (car files))
                                    (next-path (build-path dir next-file))
                                    (next-dest (build-path dest next-file)))
                               (if (real-directory? next-path)
                                   (copy-directory
                                    next-path next-dest recursive)
                                   (copy-file next-path next-dest))
                             (cpy-loop (cdr files))))))
                     ;; )))
                   (error
                    (fmt (string-append
                          "~s: Directory ~s is not empty, ignoring.\n"
                          "Try with recursive flag set.")
                         'copy-directroy dir))
                   ))))
        ((file-is-directory? dir) (copy-file dir dest))
        (else (printf "~s: ~s is not a directory, ignoring."
                      'copy-directory dir))))

(defn delete-directory (path #:optional (recursive #f))
  "(delete-directory path #:optional recursive)
   Delete directory on 'path', if 'recursive' is true delete recursively.
   Recursive defaults to #f. Does not follow symlinks."
  (cond ((real-directory? path)
         (let* ((contents (directory-files path #:hidden))
                (empty (null? contents)))
           (if empty
               (rmdir path)
               (if recursive
                   (begin (for-each
                           (lambda (x)
                             (let ((next-path (string-append path os-sep x)))
                               (if (real-directory? next-path)
                                   (delete-directory next-path recursive)
                                   (delete-file next-path))))
                           contents)
                          (rmdir path))
                   (error
                    (fmt (string-append
                          "~s: Directory ~s is not empty, ignoring.\n"
                          "To delete recursively set recursive flag to #t.")
                         'delete-directory path))))))
        ((file-is-directory? path)
         (delete-file path))
        (#:else (printf "~s: ~s is not a directory, ignoring."
                        'delete-directory path)
                #f)))

;; TODO:
(defn delete-directory-recursive (path exclusion-list #:optional (inclusive #f))
  "(delete-directory-recursive path exclusion-list #:optional inculsive)
   Delete the contents of directory 'path' recursively skipping paths in
   'exclusion-list'. Delete path as well if inclusive is true.
   Inclusive defaults to #f. Does not follow symlinks."
  (cond ((real-directory? path)
         (let* ((contents (directory-files path #:hidden))
                (empty (null? contents)))
           (if (and empty inclusive)
               (rmdir path)
               (begin (for-each
                       (lambda (x)
                         (let ((next-path (string-append path os-sep x)))
                           (if (null? (lset-intersection equal?
                                            (list x next-path) exclusion-list))
                               (if (real-directory? next-path)
                                   (begin
                                     (delete-directory-recursive next-path
                                                                 exclusion-list
                                                                 #t))
                                   (delete-file next-path)))))
                       contents)
                      (if (and inclusive
                               (null? (directory-files path #:hidden)))
                          (rmdir path))))))
        ((and inclusive (file-is-directory? path))
         (delete-file path))
        (#:else (if inclusive (format #t "~s: ~s is not a directory, ignoring"
                                      'delete-directory-recursive path))
                #f)))
