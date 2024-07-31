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
    #include <rocksdb/c.h>
    void testprintf() {
      rocksdb_options_t *opts = rocksdb_options_create();
      printf("Test function: %ld\n", (long)opts);
      rocksdb_options_destroy(opts);
    }
    // TODO: Create function to initialize RocksDB and return a structure pointer with:
    // 1. its database pointer
    // 2. its options
    // 3. its write options
    // 4. its read options

    // TODO: Create function to close RocksDB and clean up its structure:
    // 1. Cleaning its options pointer
    // 2. Cleaning its write options
    // 3. Cleaning its read options
    // 4. Closing the database pointer
    // 5. Cleaning the structure pointer, itself
____c-declare-end
  )
  (define-c-lambda testprintf () void "testprintf")
  (define-c-lambda rocksdb-options-create () (pointer void) "rocksdb_options_create")
  (define-c-lambda rocksdb-options-destroy ((pointer void)) void "rocksdb_options_destroy"))

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
