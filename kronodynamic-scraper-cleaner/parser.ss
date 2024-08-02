;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/foreign
        :std/misc/process
        :scheme/process-context)
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
        // TODO: Initialize key here
          fprintf(stderr, "get key %s\n", err);
          free(value);
          free(err);
          return -1;
      }
      // TODO: Increase value here
      value = "0";
      rocksdb_put(container->db, container->wo, key, strlen(key), value, strlen(value), &err);
      if (err != NULL) {
        fprintf(stderr, "put key %s\n", err);
        free(err);
        // free(value);
        return -1;
      }
      free(err);
      // free(value);
      return 0;
    }

    rocksdb_iterator_t *open_iterator(rocksdb_container_t *container) {
      return rocksdb_create_iterator(container->db, container->ro);
    }

    char *iterator_key(rocksdb_iterator_t *iterator) {
      return NULL;
    }

    char *iterator_value(rocksdb_iterator_t *iterator) {
      return NULL;
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
  (define-c-lambda iterator-value ((pointer void)) char-string "iterator_value")
  (define-c-lambda release ((pointer void)) void "free"))

(defclass Parser 
  (input-dir
   output-file)
  constructor: :constructor!)

(defmethod {:constructor! Parser}
  (lambda 
    (self 
     (input-dir (string-append (get-environment-variable "HOME") "/.kronodynamic/database")) 
     (output-file "./output.txt"))
       (set! (@ self input-dir) input-dir)
       (set! (@ self output-file) output-file)))

(defmethod {run Parser}
  (lambda (self)
    (let ((dpath (string-append 
                 (get-environment-variable "HOME") 
                 "/.kronodynamic/scraper-cleaner-database-file.bin")))
    (display "This is the input directory: ")
    (display (@ self input-dir))
    (display "\n")
    (display "This is the output file: ")
    (display (@ self output-file))
    (display "\n")
    (if (file-exists? dpath) (raise "Unable to parse scrape data: database file exists"))
    (let* ((db (open-database dpath)))
      (display "This is RocksDB database pointer: ")
      (display db)
      (display "Adding random key:\n")
      (display (write-key db "key"))
      (display "\n")
      (close-database db)
      (display "Database closed.\n")
      (run-process ["rm" "-Rf" dpath])
      (display "Database removed.\n")))))
