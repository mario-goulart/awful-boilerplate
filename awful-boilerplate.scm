(use posix srfi-1 srfi-13 irregex)

(define db-types '(postgresql sql-de-lite sqlite3))

(define (prompt-for prompt #!optional default valid-options)
  (let loop ()
    (newline)
    (display prompt)
    (let ((input (read-line)))
      (if (or (eof-object? input)
              (equal? (string-trim-both input) ""))
          (or default (loop))
          (if (and valid-options (not (member input valid-options)))
              (loop)
              input)))))

(define (check-db-type db-type)
  (unless (memq db-type db-types)
    (print "Invalid db-type: " db-type "\n")
    (print "Available options are: "
           (string-intersperse (map symbol->string db-types) ", "))
    (exit 1)))

(define (write-egg-file file content)
  (call-with-output-file file
    (lambda (out)
      (display content out)
      (newline out))))

(define-record egg
  name
  author
  license
  synopsis
  db-type
  name.scm
  name.so
  name.import.scm
  name.import.so)

(define %make-egg make-egg)

(define (make-egg name author license synopsis db-type)
  (%make-egg name
             author
             license
             synopsis
             db-type
             (string-append name ".scm")
             (string-append name ".so")
             (string-append name ".import.scm")
             (string-append name ".import.so")))


(define (write-setup-file dir egg-obj)
  (write-egg-file (make-pathname dir (egg-name egg-obj) "setup")
                  (sprintf
#<#EOF
(compile -d0 -O3 -J -s ~a)
(compile -d0 -O3 -s ~a)

(install-extension
 '~a
 '~S
 '((version "0.0.1")))
EOF
  (egg-name.scm egg-obj)
  (egg-name.import.scm egg-obj)
  (egg-name egg-obj)
  (list (egg-name.so egg-obj) (egg-name.import.so egg-obj)))))


(define (write-meta-file dir egg-obj)
  (write-egg-file (make-pathname dir (egg-name egg-obj) "meta")
                  (sprintf
#<#EOF
((synopsis ~S)
 (author ~S)
 (category web)
 (license ~S)
 (depends awful~a)
 (test-depends)
 (foreign-depends))
EOF
  (egg-synopsis egg-obj)
  (egg-author egg-obj)
  (egg-license egg-obj)
  (let ((db-type (egg-db-type egg-obj)))
    (if db-type
        (string-append " awful-" db-type)
        "")))))


(define (write-module-file dir egg-obj)
  (let ((name (egg-name egg-obj)))
    (write-egg-file (make-pathname dir name "scm")
                    (sprintf
#<#EOF
(module ~a (~a)
(import chicken scheme)
(include ~S)
)
EOF
  name
  name
  (make-pathname "scm" name "scm")))))


(define (write-app-main-file dir egg-obj)
  (let ((name (egg-name egg-obj))
        (db-type (egg-db-type egg-obj)))
    (write-egg-file (make-pathname (list dir "scm") name "scm")
                    (sprintf
#<#EOF
;; Core units
(use data-structures irregex)

;; Eggs
(use awful~a)

(define (~a base-path~a ##!key (awful-settings (lambda (_) (_))))

  (define base-path-pattern
    (irregex (string-append (string-chomp base-path "/") "(/.*)*")))

  (define-app ~a
    matcher: (lambda (path)
               (irregex-match base-path-pattern path))
    handler-hook: (lambda (handler)
                    (parameterize ((enable-sxml ##t)
                                   (app-root-path base-path)~a)
                      ~a(awful-settings handler)))

    (define-page (main-page-path)
      (lambda ()
        ~S))
  ))
EOF
  (if db-type
      (string-append " awful-" db-type)
      "")
  name
  (if db-type " database-credentials" "")
  name
  (if db-type
     "\n                                   (db-credentials database-credentials)"
     "")
  (if db-type
      (sprintf "(switch-to-~a-database)\n                      " db-type)
      "")
  name))))

(define (write-example-app-file dir egg-obj)
  (let ((name (egg-name egg-obj))
        (db-type (egg-db-type egg-obj)))
    (write-egg-file (make-pathname dir (string-append name "-app") "scm")
                    (sprintf
#<#EOF
(use ~a)

;; While developing, you may want to
;;   (load ~S)
;; instead of (use ~a)

~a(~a "/"~a)
EOF
  name
  (make-pathname "scm" name "scm")
  name
  (if db-type
      (case (string->symbol db-type)
        ((postgresql)
#<#EOF
(define credentials
  '((dbname . "the-database-name")
    (user . "user")
    (password . "password")
    (host . "localhost")))


EOF
        )
        ((sqlite3 sql-de-lite)
#<#EOF
(define db-file "the-db-file.db")


EOF
        ))
    "")
  name
  (if db-type
     (string-append " "
                    (case (string->symbol db-type)
                      ((postgresql) "credentials")
                      ((sqlite3 sql-de-lite) "db-file")))
     "")))))


(define (create-work-dir dir license db-type)
  (create-directory dir 'with-parents)
  (create-directory (make-pathname dir "css"))
  (create-directory (make-pathname dir "js"))
  (create-directory (make-pathname dir "conf"))
  (create-directory (make-pathname dir "scm"))
  (let* ((egg-name (pathname-file dir))
         (egg-license
          (or license
              (prompt-for
               "License (default is \"BSD\", same as Chicken's): " "BSD")))
         (db-type (or db-type
                      (prompt-for
                       (string-append
                        "Database type (options: "
                        (string-intersperse (map symbol->string db-types) ", ")
                        " or blank for if your app doesn't use a database): ")
                       ""
                       (cons "" (map symbol->string db-types)))))
         (db-type (if (equal? db-type "") #f db-type))
         (egg-author (prompt-for "Your name: "))
         (egg-synopsis (prompt-for "Application synopsis: "))
         (egg-obj (make-egg egg-name egg-author egg-license egg-synopsis db-type)))
    (when db-type
      (create-directory (make-pathname dir "sql")))
    (write-setup-file dir egg-obj)
    (write-meta-file dir egg-obj)
    (write-module-file dir egg-obj)
    (write-app-main-file dir egg-obj)
    (write-example-app-file dir egg-obj)
    (print dir " has been created.")))


(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (lambda (arg)
                    (irregex-match
                     `(seq ,(->string option) "=" (submatch (* any)))
                     arg))
                  args)))
    (and val (irregex-match-substring val 1))))


(define (usage #!optional exit-code)
  (print "Usage: " (pathname-strip-directory (program-name))
         " [OPTION ...] NAME

Create a work directory named NAME for an awful application.

Options:
--license=<license>
    the license the awful application can be copied and used under

--db-type=<db-type>
    if your app uses a database, use one of the following values:")

  (for-each (lambda (db-type)
              (print "        - " db-type))
            db-types)
(print "
-h, --help
    show this help")
  (when exit-code (exit exit-code)))



;;;
;;; Command line parsing
;;;
(let ((args (command-line-arguments)))

  (when (null? args)
    (usage 1))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (let* ((non-option-args
          (remove (lambda (arg)
                    (string-prefix? "--" arg))
                  args))
         (license (cmd-line-arg '--license args))
         (db-type (and-let* ((db-type (cmd-line-arg '--db-type args)))
                    (string->symbol db-type))))

    (when db-type
      (check-db-type db-type))

    (when (null? non-option-args)
      (usage 1))

    (let ((dir (car non-option-args)))
      (when (file-exists? dir)
        (print "App directory already exists: " dir)
        (exit 1))
      (create-work-dir dir license db-type))))
