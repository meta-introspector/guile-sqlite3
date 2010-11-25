;; Guile-SQLite3
;; Copyright (C) 2010 Andy Wingo <wingo at pobox dot com>

;; This library is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;                                                                  
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;                                                                  
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; A Guile binding for sqlite.
;;
;;; Code:

(define-module (sqlite3)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (sqlite-open
            sqlite-close))

;;
;; Utils
;;
(define (string->utf8-pointer s)
  (bytevector->pointer (string->utf8 s)))

(define strlen
  (pointer->procedure size_t
                      (dynamic-pointer "strlen" (dynamic-link))
                      '(*)))

(define (utf8-pointer->string p)
  (utf8->string (pointer->bytevector p (strlen p))))


;;
;; Constants
;;

;; FIXME: snarf using compiler. These are just copied from the header...
;;
(define SQLITE_OPEN_READONLY         #x00000001) ;; Ok for sqlite3_open_v2()
(define SQLITE_OPEN_READWRITE        #x00000002) ;; Ok for sqlite3_open_v2()
(define SQLITE_OPEN_CREATE           #x00000004) ;; Ok for sqlite3_open_v2()
(define SQLITE_OPEN_DELETEONCLOSE    #x00000008) ;; VFS only
(define SQLITE_OPEN_EXCLUSIVE        #x00000010) ;; VFS only
(define SQLITE_OPEN_MAIN_DB          #x00000100) ;; VFS only
(define SQLITE_OPEN_TEMP_DB          #x00000200) ;; VFS only
(define SQLITE_OPEN_TRANSIENT_DB     #x00000400) ;; VFS only
(define SQLITE_OPEN_MAIN_JOURNAL     #x00000800) ;; VFS only
(define SQLITE_OPEN_TEMP_JOURNAL     #x00001000) ;; VFS only
(define SQLITE_OPEN_SUBJOURNAL       #x00002000) ;; VFS only
(define SQLITE_OPEN_MASTER_JOURNAL   #x00004000) ;; VFS only
(define SQLITE_OPEN_NOMUTEX          #x00008000) ;; Ok for sqlite3_open_v2()
(define SQLITE_OPEN_FULLMUTEX        #x00010000) ;; Ok for sqlite3_open_v2()
(define SQLITE_OPEN_SHAREDCACHE      #x00020000) ;; Ok for sqlite3_open_v2()
(define SQLITE_OPEN_PRIVATECACHE     #x00040000) ;; Ok for sqlite3_open_v2()

(define libsqlite3 (dynamic-link "libsqlite3"))

(define-record-type <sqlite-db>
  (make-sqlite-db pointer open?)
  sqlite-db?
  (pointer sqlite-db-pointer)
  (open? sqlite-db-open? set-sqlite-db-open?!))

(define sqlite-errmsg
  (let ((f (pointer->procedure
            '*
            (dynamic-func "sqlite3_errmsg" libsqlite3)
            (list '*))))
    (lambda (db)
      (utf8-pointer->string (f (sqlite-db-pointer db))))))

(define sqlite-errcode
  (let ((f (pointer->procedure
            int
            (dynamic-func "sqlite3_extended_errcode" libsqlite3)
            (list '*))))
    (lambda (db)
      (f (sqlite-db-pointer db)))))

(define* (sqlite-error db who #:optional code
                       (errmsg (and db (sqlite-errmsg db))))
  (throw 'sqlite-error who code errmsg))

(define* (check-error db #:optional who)
  (let ((code (sqlite-errcode db)))
    (if (not (zero? code))
        (sqlite-error db who code))))

(define sqlite-close
  (let ((f (pointer->procedure
            int
            (dynamic-func "sqlite3_close" libsqlite3)
            (list '*))))
    (lambda (db)
      (if (sqlite-db-open? db)
          (begin
            (let ((p (sqlite-db-pointer db)))
              (set-sqlite-db-open?! db #f)
              (f p)))))))

(define db-guardian (make-guardian))
(define (pump-db-guardian)
  (let ((c (db-guardian)))
    (if c
        (begin
          (sqlite-close c)
          (pump-db-guardian)))))
(add-hook! after-gc-hook pump-db-guardian)

(define (static-errcode->errmsg code)
  (case code
    ((1) "SQL error or missing database")
    ((2) "Internal logic error in SQLite")
    ((3) "Access permission denied")
    ((5) "The database file is locked")
    ((6) "A table in the database is locked")
    ((7) "A malloc() failed")
    ((8) "Attempt to write a readonly database")
    ((10) "Some kind of disk I/O error occurred")
    ((11) "The database disk image is malformed")
    ((14) "Unable to open the database file")
    ((21) "Library used incorrectly")
    ((22) "Uses OS features not supported on host")
    ((23) "Authorization denied")
    ((24) "Auxiliary database format error")
    ((26) "File opened that is not a database file")
    (else "Unknown error")))

(define sqlite-open
  (let ((f (pointer->procedure
            int
            (dynamic-func "sqlite3_open_v2" libsqlite3)
            (list '* '* int '*))))
    (lambda* (filename #:optional (flags 0) (vfs #f))
      (let* ((out-db (bytevector->pointer (make-bytevector (sizeof '*) 0)))
             (ret (f (string->utf8-pointer filename)
                     out-db
                     flags
                     (if vfs (string->utf8-pointer vfs) %null-pointer))))
        (if (zero? ret)
            (let ((c (make-sqlite-db (dereference-pointer out-db) #t)))
              (db-guardian c)
              c)
            (sqlite-error #f 'sqlite-open ret (static-errcode->errmsg ret)))))))

