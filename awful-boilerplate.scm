(use posix srfi-1 srfi-13 irregex)

(define db-types '(postgresql sql-de-lite sqlite3))

;; Some code stolen from chicken-hatch
(define (prompt-for prompt #!optional default valid-options)
  (let loop ()
    (newline)
    (display prompt)
    (let ((input (read-line)))
      (if (or (eof-object? input)
              (equal? (string-trim-both input) ""))
          (if (and valid-options (not (member input valid-options)))
              (loop)
              (or default (loop)))
          input))))

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

(define (write-string x)
  (with-output-to-string (lambda () (write x))))

(define (create-work-dir dir license db-type)
  (create-directory dir 'with-parents)
  (create-directory (make-pathname dir "css"))
  (create-directory (make-pathname dir "js"))
  (create-directory (make-pathname dir "conf"))
  (create-directory (make-pathname dir "scm"))
  (let* ((egg-name (pathname-file dir))
         (egg-license
          (or license
              (prompt-for "License (default is \"BSD\", same as Chicken's): " "BSD")))
         (db-type (or db-type
                      (prompt-for
                       (string-append "Database type (options: "
                                      (string-intersperse (map symbol->string db-types) ", ")
                                      " or blank for if your app doesn't use a database): ")
                       ""
                       (cons "" (map symbol->string db-types)))))
         (db-type (if (equal? db-type "") #f db-type))
         (egg-author (prompt-for "Your name: "))
         (egg-synopsis (prompt-for "Application synopsis: "))
         (egg-symbol (string->symbol egg-name))
         (egg-name.scm (string-append egg-name ".scm"))
         (egg-name.so (string-append egg-name ".so"))
         (egg-name.import.scm (string-append egg-name ".import.scm"))
         (egg-name.import.so (string-append egg-name ".import.so")))
    (when db-type
        (create-directory (make-pathname dir "sql")))
    (write-egg-file (make-pathname dir egg-name "setup")
#<#EOF
(compile -d0 -O3 -J -s #(write-string (string->symbol egg-name.scm)))
(compile -d0 -O3 -s #(write-string (string->symbol egg-name.import.scm)))

(install-extension
 '#(write-string egg-symbol)
 '#(write-string (list egg-name.so egg-name.import.so))
 '((version "0.0.1")))
EOF
)

    (write-egg-file (make-pathname dir egg-name "meta")
#<#EOF
((synopsis #(write-string egg-synopsis))
 (author #(write-string egg-author))
 (category web)
 (license #(write-string egg-license))
 (depends awful #(if db-type db-type ""))
 (test-depends)
 (foreign-depends))
EOF
)


(write-egg-file (make-pathname dir egg-name "scm")
#<#EOF
(module #(write-string egg-symbol) (#(write-string egg-symbol))
(import chicken scheme)
#(string-append "(include \"" (make-pathname "scm" egg-name "scm") "\")")
)
EOF
)

(write-egg-file (make-pathname (list dir "scm") egg-name "scm")
#<#EOF
(use srfi-13 awful #(if db-type (string-append "awful-" db-type) ""))

(define (#(write-string egg-symbol) base-path #(if db-type 'database-credentials "") ##!key (awful-settings (lambda (_) (_))))

  (add-request-handler-hook!
   '#(write-string egg-symbol)
   (lambda (path handler)
     (when (string-prefix? base-path path)
       (parameterize ((enable-sxml ##t)
                      (app-root-path base-path)
                      (db-credentials database-credentials))
         #(if db-type `(,(string->symbol (conc "switch-to-" db-type "-database"))) "")
         (awful-settings handler)))))

  ;; write the code of your app here
  )
EOF
)

(write-egg-file (make-pathname dir (string-append egg-name "-app") "scm")
#<#EOF
(use #(write-string egg-symbol))

;; While developing, you may want to
;;   #(write-string `(load ,(make-pathname "scm" egg-name "scm")))
;; instead of (use #(write-string egg-symbol))

#(if (equal? db-type "postgresql")
     (string-append
      "(define credentials '"
      (write-string
       '((dbname . "the-database-name")
         (user . "user")
         (password . "password")
         (host . "localhost")))
      ")\n")
     "")
(#egg-name "/" #(if db-type
                    (case (string->symbol db-type)
                      ((postgresql) "credentials")
                      ((sqlite3 sql-de-lite) (write-string "the-db-file.db"))
                      (else ""))
                    ""))
EOF
)
  (print dir " has been created.")
  ));; end create-work-dir


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
