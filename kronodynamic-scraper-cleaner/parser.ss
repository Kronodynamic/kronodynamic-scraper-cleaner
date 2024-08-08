;;; -*- Gerbil -*-
(import :scheme/process-context
        :std/error
        :std/foreign
        :std/misc/ports
        :std/misc/process
        :std/sort
        :std/sugar)
(export #t)

;; External FFI declaration:
(begin-ffi (open-database 
            close-database 
            write-key 
            open-iterator 
            close-iterator 
            iterator-first
            iterator-next
            iterator-valid
            iterator-key
            iterator-value
            release)
  (c-declare #<<____c-declare-end
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <rocksdb/c.h>
    
    // Opaque struct to store RocksDB data state
    typedef struct {
      rocksdb_options_t *opts;
      rocksdb_writeoptions_t *wo;
      rocksdb_readoptions_t *ro;
      rocksdb_t *db;
      int key_id;
            
    } rocksdb_container_t;

    rocksdb_container_t *open_database(char *db_name) {
      rocksdb_options_t *opts = rocksdb_options_create();
      rocksdb_options_set_create_if_missing(opts, 1);
      rocksdb_options_set_error_if_exists(opts, 1);
      rocksdb_options_set_compression(opts, rocksdb_snappy_compression);
      char *err = NULL;
      rocksdb_t *db = rocksdb_open(opts, db_name, &err);
      if (err != NULL) {
        free(err);
        free(opts);
        return NULL;
      }
      rocksdb_container_t *container = (rocksdb_container_t*) malloc(sizeof(rocksdb_container_t));
      container->opts = opts;
      container->wo = rocksdb_writeoptions_create();
      container->ro = rocksdb_readoptions_create();
      container->db = db;
      container->key_id = 0;
      return container;
    }
    
    void close_database(rocksdb_container_t *container) {
      rocksdb_writeoptions_destroy(container->wo);
      rocksdb_readoptions_destroy(container->ro);
      rocksdb_options_destroy(container->opts);
      rocksdb_close(container->db);
      free(container);
    }

    int write_key(rocksdb_container_t *container, char *key) {
      char *err = NULL;  
      size_t rlen = 0;
      char *value = rocksdb_get(container->db, container->ro, key, strlen(key), &rlen, &err);
      if (err != NULL) {
          free(value);
          free(err);
          return -1;
      }

      if(rlen == 0) {
        free(value);
        value = "0";
        rocksdb_put(container->db, container->wo, key, strlen(key), value, strlen(value), &err);
        if(err != NULL) {
          free(err);
          return -1;
        }
        return 0;
      }

      char *res = (char*) malloc(rlen+1);
      memset(res, 0, rlen+1);
      memcpy(res, value, rlen);
      long lvalue = atol(res);
      free(value);
      free(res);

      res = (char*) malloc(250);
      memset(res, 0, 250);
      lvalue++;
      sprintf(res, "%ld", lvalue);
      rocksdb_put(container->db, container->wo, key, strlen(key), res, strlen(res), &err);
      free(res);

      if (err != NULL) {
        free(err);
        return -1;
      }

      return lvalue;
    }

    rocksdb_iterator_t *open_iterator(rocksdb_container_t *container) {
      return rocksdb_create_iterator(container->db, container->ro);
    }

    char *iterator_key(rocksdb_iterator_t *iterator) {
      size_t klen = 0;
      const char *key = rocksdb_iter_key(iterator, &klen);
      char *res = (char*) malloc(klen+1);
      memset(res, 0, klen+1);
      memcpy(res, key, klen);
      return res;
    }

    long iterator_value(rocksdb_iterator_t *iterator) {
      size_t vlen = 0;
      const char *value = rocksdb_iter_value(iterator, &vlen);
      char *res = (char*) malloc(vlen+1);
      memset(res, 0, vlen+1);
      memcpy(res, value, vlen);
      long lres = atol(res);
      free(res);
      return lres;
    }
____c-declare-end
  )
  (define-c-lambda open-database (nonnull-char-string) (pointer void) "open_database")
  (define-c-lambda close-database ((pointer void)) void "close_database")
  (define-c-lambda write-key ((pointer void) nonnull-char-string) int "write_key")
  (define-c-lambda open-iterator ((pointer void)) (pointer void) "open_iterator")
  (define-c-lambda close-iterator ((pointer void)) void "rocksdb_iter_destroy")
  (define-c-lambda iterator-first((pointer void)) void "rocksdb_iter_seek_to_first")
  (define-c-lambda iterator-next ((pointer void)) void "rocksdb_iter_next")
  (define-c-lambda iterator-valid ((pointer void)) unsigned-char "rocksdb_iter_valid")
  (define-c-lambda iterator-key ((pointer void)) char-string "iterator_key")
  (define-c-lambda iterator-value ((pointer void)) long "iterator_value"))

(defclass Parser 
  (input-dir
   output-file
   dpath)
  constructor: :constructor!)

(defmethod {:constructor! Parser}
  (lambda 
    (self 
     (input-dir (string-append (get-environment-variable "HOME") "/.kronodynamic/database")) 
     (output-file "./output.txt")
     (dpath (string-append
              (get-environment-variable "HOME") 
              "/.kronodynamic/scraper-cleaner-database-file.bin")))
       (set! (@ self input-dir) input-dir)
       (set! (@ self output-file) output-file)
       (set! (@ self dpath) dpath)))

(defmethod {run Parser}
  (lambda (self)
    (let ((dpath (@ self dpath)))
    (display "This is the input directory: ")
    (display (@ self input-dir))
    (display "\n")
    (display "This is the output file: ")
    (display (@ self output-file))
    (display "\n")
    (if (file-exists? dpath) (raise "Unable to parse scrape data: database file exists"))
    (let* ((db (open-database dpath)))
      {process-input-dir self db}
      #;(begin
        (display "This is RocksDB database pointer: ")
        (display db)
        (display "Adding random key:\n")
        (display (write-key db "key"))
        (display "\n")
        (display (write-key db "key"))
        (display "\n")
        (display (write-key db "keyes"))
        (display "\n")
        (let ((it (open-iterator db)))
          (display "Iterator: ")
          (display it)
          (display "\n")
          (iterator-first it)
          (let ((key (iterator-key it)))
            (display "This is current key: ")
            (display key)
            (display "\n"))
          (iterator-next it)
          (let ((key (iterator-key it)))
            (display "This is current key: ")
            (display key)
            (display "\n"))
          (close-iterator it)))
      (close-database db)
      (display "Database closed.\n")
      (run-process ["rm" "-Rf" dpath])
      (display "Database removed.\n")))))

(defmethod {process-input-dir Parser}
  (lambda (self db)
    (let* ((input-dir (@ self input-dir))
           (files (sort (directory-files input-dir) string<?)))
      (let loop ((cfile (car files)) (cfiles (cdr files)))
        {process-input-file self db (string-append input-dir "/" cfile)}
        (if (not (null? cfiles))
          (loop (car cfiles) (cdr cfiles)))))))

(defmethod {process-input-file Parser}
  (lambda (self db file)
    (run-process 
      ["bunzip2" "--stdout" file] 
      coprocess: 
      (lambda (process)
        (let loop ((l (read-line process)))
          (if (not (eof-object? l))
            (begin
              (display file)
              (display ":")
              (display l)
              (display "\n")
              ;; TODO: Parse JSON Object here.
              (loop (read-line process)))))))
    (display "\n\n")))
