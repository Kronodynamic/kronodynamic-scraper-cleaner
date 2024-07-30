;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/cli/getopt
        ./lib)
(export main)

;; build manifest; generated during the build
;; defines version-manifest which you can use for exact versioning
(include "../manifest.ss")

(def (main . args)
  (call-with-getopt kronodynamic-scraper-cleaner-main aargs
    program: "kronodynamic-scraper-cleaner"
    help: "A one line description of your program"
    ;; commands/options/flags for your program; see :std/cli/getopt
    (command 'parse help: "Scan and parse "
      (argument
        'directory 
        help: "Directory to search and parse scraper output"))))

(def* kronodynamic-scraper-cleaner-main
  ((opt)
   (kronodynamic-scraper-cleaner-main/options opt))
  ((cmd opt)
   (kronodynamic-scraper-cleaner-main/command cmd opt)))

;;; Implement this if your CLI has commands
(def (kronodynamic-scraper-cleaner-main/command cmd opt)
  (display "Implement me, command!\n"))
