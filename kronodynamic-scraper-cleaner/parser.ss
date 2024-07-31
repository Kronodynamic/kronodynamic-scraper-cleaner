;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/foreign
        :scheme/process-context)
(export #t)

;; External FFI declaration:
(begin-ffi (testprintf
            rocksdb-options-create
            rocksdb-options-destroy
            rocksdb-options-t
            rocksdb-options-t*)
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

    // TODO: Create function to write key to RocksDB and increase its value. Return its value
    int write_key(rocksdb_container_t *container, char *key) {
      return 0;
    }

    // TODO: Create functions to list (all) keys from RocksDB
    rocksdb_iterator_t *open_iterator(rocksdb_container_t *container) {
      return NULL;
    }

    void close_iterator(rocksdb_container_t *container, rocksdb_iterator_t *iterator) {
    }

    char *next_key(rocksdb_iterator_t *iterator) {
      return NULL;
    }

    void clear_key(char *key) {
      free(key);
    }
____c-declare-end
  )
  (define-c-lambda open-database (nonnull-char-string) (pointer void) "open_database")
  (define-c-lambda close-database ((pointer void)) void "close_database")
  (define-c-lambda write-key ((pointer void) nonnull-char-string) int "write_key")
  (define-c-lambda open-iterator ((pointer void)) (pointer void) "open_iterator")
  (define-c-lambda close-iterator ((pointer void) (pointer void)) void "close_iterator")
  (define-c-lambda next-key ((pointer void)) char-string "next_key")
  (define-c-lambda clear-key (nonnull-char-string) void "clear_key"))

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
    (display "This is the input directory: ")
    (display (@ self input-dir))
    (display "\n")
    (display "This is the output file: ")
    (display (@ self output-file))
    (display "\n")
    (let ((p (rocksdb-options-create)))
      (display "This is RocksDB pointer: ")
      (display p)
      (display "\n")
      (rocksdb-options-destroy p)
      (display "This is RocksDB cleaned pointer: ")
      (display p)
      (display "\n"))))
