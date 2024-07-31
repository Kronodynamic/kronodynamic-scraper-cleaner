;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/cli/getopt
        ./parser)
(export main)

;; build manifest; generated during the build
;; defines version-manifest which you can use for exact versioning
(include "../manifest.ss")

(def (main . args)
  (call-with-getopt kronodynamic-scraper-cleaner-main args
    program: "kronodynamic-scraper-cleaner"
    help: "A one line description of your program"
    ;; commands/options/flags for your program; see :std/cli/getopt
    (command 'parse help: "Scan and parse "
      (argument
        'directory 
        help: "Directory to search and parse scraper output"))))

(def* kronodynamic-scraper-cleaner-main
  ((cmd opt)
   (let 
    ((p (Parser))) 
    {run p})))
