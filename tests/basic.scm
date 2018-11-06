;;;; basic.scm ---      -*- mode: scheme; coding: utf-8; -*-
;;;;
;;;;   Copyright (C) 2011 Detlev Zundel <dzu@denx.de>
;;;;   Copyright (C) 2018 Ludovic Courtès <ludo@gnu.org>
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (tests basic-test)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:use-module (sqlite3))

(define (sqlite-exec* db sql key value)
  (let ((stmt (sqlite-prepare db sql)))
    (sqlite-bind stmt key value)
    (sqlite-map display stmt)
    (sqlite-finalize stmt)
    #t))

;; Cleanup database so we can check creation
(define db-name "tests/simple.db")
(if (file-exists? db-name)
    (begin
      (format #t "Removing leftover database ~a~%" db-name)
      (delete-file db-name)))

(define db
  ;; Global database used for tests.
  #f)


(test-begin "basic")

(test-assert "sqlite-open"
  (begin
    (set! db (sqlite-open db-name (logior SQLITE_OPEN_CREATE
                                          SQLITE_OPEN_READWRITE)))
    (sqlite-db? db)))

(test-assert "sqlite-busy-timeout"
  (sqlite-busy-timeout db 20))

(test-assert "create table"
  (sqlite-exec db
               "create table project (
      reference integer primary key,
      name   text,
      website   text
  )"))

(test-assert "insert"
  (sqlite-exec db
               "insert into project values (1, 'Guile', '');
                insert into project values (2, 'Guix', 'gnu.org');"))

(test-assert "sqlite-prepare with caching"
  (let* ((s "SELECT * FROM project")
         (stmt (sqlite-prepare db s #:cache? #t)))
    (eq? stmt (sqlite-prepare db s #:cache? #t))))

(test-equal "select"
  '(#(1 "Guile" "") #(2 "Guix" "gnu.org"))
  (let* ((stmt   (sqlite-prepare db "select * from project"))
         (result (sqlite-map identity stmt)))
    (sqlite-finalize stmt)
    result))

(test-assert "select with named parameters"
  (sqlite-exec* db "select * from project where 'bla' = :foo" ":foo" "bla"))

(test-assert "select with named parameters, alternate form"
  (sqlite-exec* db "select * from project where 'bla' = :foo" 'foo "bla"))

(test-assert "insert with sqlite-bind"
  (begin
    (sqlite-exec db "CREATE TABLE foos (dbid INTEGER PRIMARY KEY, name TEXT)")
    (let ((stmt (sqlite-prepare db "INSERT INTO foos(name) VALUES(?)")))
      (sqlite-bind stmt 1 "myfoo")
      (sqlite-step stmt)
      (sqlite-finalize stmt)
      #t)))

(test-assert "drop"
  (sqlite-exec db "DROP TABLE IF EXISTS foos"))

(sqlite-close db)
(delete-file db-name)

(test-end "basic")
