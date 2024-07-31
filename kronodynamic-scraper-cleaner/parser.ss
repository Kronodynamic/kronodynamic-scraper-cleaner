;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/foreign
        :scheme/process-context)
(export #t)

;; External FFI declaration:
(begin-ffi (testprintf)
  (c-declare #<<____c-declare-end
    #include <stdio.h>
    #include <rocksdb/c.h>
    void testprintf() {
      rocksdb_options_t *opts = rocksdb_options_create();
      printf("Test function: %ld", (long)opts);
      rocksdb_options_destroy(opts);
    }
____c-declare-end
  )
  (define-c-lambda testprintf () void "testprintf"))

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
    (display (@ self output-file))
    (display "\n")
    (testprintf)))
