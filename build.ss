#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("kronodynamic-scraper-cleaner/lib"
    (exe: "kronodynamic-scraper-cleaner/main" bin: "kronodynamic-scraper-cleaner")))
