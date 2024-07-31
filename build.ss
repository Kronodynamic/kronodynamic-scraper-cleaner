#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("kronodynamic-scraper-cleaner/parser"
    (exe: "kronodynamic-scraper-cleaner/main" 
          bin: "kronodynamic-scraper-cleaner" 
          "-ld-options"  "-L /usr/local/lib -lstdc++ -lsnappy -lbz2 -llz4 -lz -lrocksdb")))
