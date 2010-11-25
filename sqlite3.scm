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

(define-record-type <sqlite-connection>
  (make-sqlite-connection pointer open?)
  sqlite-connection?
  (pointer sqlite-connection-pointer)
  (open? sqlite-connection-open? set-sqlite-connection-open?!))

(define sqlite-close
  (let ((f (pointer->procedure
            int
            (dynamic-func "sqlite3_close" libsqlite3)
            (list '*))))
    (lambda (connection)
      (if (sqlite-connection-open? connection)
          (begin
            (let ((p (sqlite-connection-pointer connection)))
              (set-sqlite-connection-open?! connection #f)
              (f p)))))))

(define connection-guardian (make-guardian))
(define (pump-connection-guardian)
  (let ((c (connection-guardian)))
    (if c
        (begin
          (sqlite-close c)
          (pump-connection-guardian)))))
(add-hook! after-gc-hook pump-connection-guardian)

(define (string->utf8-pointer s)
  (bytevector->pointer (string->utf8 s)))

(define strlen
  (pointer->procedure size_t
                      (dynamic-pointer "strlen" (dynamic-link))
                      '(*)))

(define (utf8-pointer->string p)
  (utf8->string (pointer->bytevector p (strlen p))))

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
            (let ((c (make-sqlite-connection (dereference-pointer out-db) #t)))
              (connection-guardian c)
              c)
            (error "error opening database" ret))))))
