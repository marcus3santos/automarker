;;;; automarker.lisp

(in-package #:automarker)

;; Unit test macros

(defvar *results* nil)

(defvar *test-name* nil)

;; Maximum running time (in seconds) allotted to the
;; evaluation of a test case. Once that time expires, the respective
;; thread is terminated, and a fail is associated to the respective
;; test case 

(defvar *max-time* 1) 

(defvar *runtime-error* nil)

(defvar *load-error* nil)

(defvar *cr-warning* nil)

(defstruct submission
  std-name
  date
  evaluation)

(defun calc-mark (res ws)
  "Calculates student mark based on the results (res) from running the test 
cases and on the weight associated to each function. If ws is nil then
the mark is calculated as the # of passes divided by the total # of cases.
- ws is a list of pairs (<function-name> <weight>) Note: sum of weights must be 100
- res is the list stored in the global variable *results*"
  (labels ((get-avg (fn res accumPass accumT)
	     (dolist (x res (if (zerop accumT)
				(error "Test function ~S not defined in unit test" fn)
				(/ accumPass accumT)))
	       (when (equal fn (caddr x))
		 (if (car x)
		     (progn (incf accumPass)
			    (incf accumT))
		     (incf accumT))))))
    (if (null ws)
	(loop for r in res
	      when (car r)
		sum 1 into c
	      finally (return (* (/ c (length res)) 100)))
	(loop for w in ws 
	      sum (* (cadr w) (get-avg (car w) res 0 0))))))

(defun change-results-readable (results)
  (when results
    (let* ((result (car results))
	   (pass-fail (car result)))
      (cons (cons (if pass-fail "Pass" "Fail") (cdr result))
	    (change-results-readable (cdr results))))))

(defun rewrite-load (file)
  "Gets rid of CR characters in file creating new file, signals a warning, loads new file,
   and deletes it (if LOAD does not throw an error)"
  (let ((newfname (concatenate 'string (directory-namestring file) (string (gensym)))))
    (with-open-file (in file)
      (setf *cr-warning* newfname)
      (with-open-file (out newfname :direction :output)
        (do ((c (read-char in) (read-char in nil 'eof)))
            ((not (characterp c)))
          (if (char= c #\Return)
              (write-char #\Newline out)
              (write-char c out)))))
    (load newfname)
    (delete-file newfname)))
  

(defun has-cr? (file)
  (with-open-file (in file)
    (do ((c (read-char in) (read-char in nil 'eof)))
        ((not (characterp c)))
      (when (char= c #\Return)
        (return t)))))

        
(defun load-solution (file)
  (if (has-cr? file)
      (rewrite-load file)
      (load file)))

(defun mark-std-solution (student-solution test-cases-dir &optional (ws nil))
  "Loads the student-solution file, loads the test cases, runs
  the test cases, and returns the percentage of correct results over total results"
  (let ((description "No runtime errors"))
    (progn
      (setf *results* nil)
      (setf *runtime-error* nil)
      (setf *load-error* nil)
      (setf *cr-warning* nil)
      (handler-case (load-solution student-solution)
        (error (condition)
          (when (and *cr-warning* (probe-file *cr-warning*))
            (delete-file *cr-warning*))
	  (push condition *load-error*)))
      (load (merge-pathnames (asdf:system-source-directory :automarker) "macros.lisp"))
      (load test-cases-dir)
      (list (format nil "~f" (calc-mark *results* ws))
	    (cond (*runtime-error* (setf description "Runtime error")
				   'runtime-error)
		  (*load-error* (setf description "Load/Compiling error")
				'load-error)
                  (*cr-warning* (setf description "CR character warning! Student's lisp file contains a CR character. New temporary file generated, loaded, and deleted.")
                                'cr-warning)
		  (t 'ok))
            (cond ((or *runtime-error* *load-error*)
                   (setf description (concatenate 'string
                                                  description
                                                  (format nil "(s) when evaluating the following expressions:~%~{- ~A~%~}" (reverse *runtime-error*)))))
                  (*cr-warning* description)
                  (t "No runtime errors"))
	    (change-results-readable *results*)))))

(defun splice-at-char (s c)
  (let ((i (position c s :from-end 0)))
    (values (subseq (subseq s i) 1) i)))

(defun form-date-time (date)
  (with-input-from-string (in date)
    (let ((month (read in))
	  (day (prog1 (read in)
		 (read in)))
	  (time (read in))
	  (period (read in)))
      (list month day time period))))

(defun month->number (name)
  (case name
    (Jan 1)
    (Feb 2)
    (Mar 3)
    (Apr 4)
    (May 5)
    (Jun 6)
    (Jul 7)
    (Aug 8)
    (Sep 9)
    (Oct 10)
    (Nov 11)
    (Dec 12)
    (otherwise (error "You have provided an incorrect month abbreviation: ~a" name))))

(defun month->seconds (m)
  (* (month->number m) 30 24 60 60))

(defun day->sec (d)
  (if (and (>= d 1) (<= d 31))
      (* d 24 60 60)
      (error "You have provided an invalid day: ~a" d)))

(defun date->seconds (date)
  (+ (month->seconds (car date)) (day->sec (cadr date))))

(defun time-ampm->time-24hrs (time)
  (let ((h (car time))
	(p (cadr time)))
    (if (and (or (eq p 'am) (eq p 'pm))
             (>= h 0)
             (<= h 1259))
        (if (or (eq p 'am) 
                (and (eq p 'pm) (>= h 1200) (<= h 1259)))
            h
            (+ h 1200))
        (error "You have provided an invalid time: ~a" time))))
	  
(defun >time (tp1 tp2)
  (let ((t1 (time-ampm->time-24hrs tp1))
	(t2 (time-ampm->time-24hrs tp2)))
    (> t1 t2)))

(defun >date (dt1 dt2)
  "True if datetime d1 is greater than d2. Both are in 
the format (Nov 5 102 AM)"
  (let* ((date1 (list (car dt1) (cadr dt1)))
	 (time1 (cddr dt1))
	 (date2 (list (car dt2) (cadr dt2)))
	 (time2 (cddr dt2))
	 (datesec1 (date->seconds date1))
	 (datesec2 (date->seconds date2)))
    (or (> datesec1 datesec2)
	(and (= datesec1 datesec2)
	     (>time time1 time2)))))
	 
(defun check-dt (dt)
  (let ((m (car dt))
	(d (cadr dt))
	(td (if (<= (caddr dt) 12) (* (caddr dt) 100) (caddr dt)))
	(p (cadddr dt)))
    (list m d td p)))


(defun create-folder-ifzipped (is-zipped submissions-dir unzipped-subs-folder)
  (if is-zipped
      (progn
        (ensure-directories-exist unzipped-subs-folder :verbose T)
	(zip:unzip submissions-dir unzipped-subs-folder :if-exists :supersede)
	unzipped-subs-folder)
      (check-foldername submissions-dir)))

(defun get-key-and-date (folder)
  (let* ((s (namestring folder))
	 (sn (subseq s 0 (- (length s) 1)))
	 (pre (splice-at-char sn #\/)))
    (multiple-value-bind (date p2) (splice-at-char pre #\-)
	(values (subseq pre 0 p2) (subseq date 1)))))

(defun invalid-submitted-file (file)
  (not (equal (pathname-type file) "lisp")))

(defun get-date-time()
  (let ((day-names '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
    (multiple-value-bind
          (second minute hour date month year day-of-week dst-p tz)
	(get-decoded-time)
      dst-p
      (format nil "It is now ~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
	      hour
	      minute
	      second
	      (nth day-of-week day-names)
	      month
	      date
	      year
	      (- tz)))))

(defun evaluate-solution (student-solution ddate sdate test-cases ws)
  (cond ((null student-solution)
         (list "0" 'no-submitted-file nil "No submitted file"))
        ((invalid-submitted-file student-solution)
         (list "0" 'not-lisp-file nil "Not a lisp file"))
        ((>date sdate ddate)
         (list "0" 'late-submission nil  (format nil "Late submission. Assignment due on: ~a Submitted on: ~a~%" ddate sdate)))
        (t (mark-std-solution student-solution test-cases ws))))
        
(defun generate-std-feedback (key eval feedback-folder)
  (let* ((fname (concatenate 'string (subseq key 0 (1- (length key))) ".txt"))
	 (folder (ensure-directories-exist (concatenate 'string  (namestring feedback-folder) fname))))
    (with-open-file (out folder
			 :direction :output :if-exists :supersede)
      (let ((error-type (second eval))
	    (descr (third eval))
	    (res (fourth eval)))
	(format out "Feedback on your assignment solution")
	(cond ((or (equal error-type 'no-submitted-file)
		   (equal error-type 'not-lisp-file)
		   (equal error-type 'late-submission)) (format out "~%~%~A" res))
	      ((equal error-type 'ok) (format out "~%~%Unit test results:~%~{- ~S~%~}" res))
	      (t (format out "~%~%~A~%~%Unit test results:~%~{- ~S~%~}" descr res)))))))

(defun get-std-name (csv)
  (let* ((pref1 (subseq csv (1+ (position #\, csv))))
	 (pref2 (subseq pref1 (1+ (position #\, pref1))))
	 (lname (subseq pref1 0 (position #\, pref1)))
	 (fname (subseq pref2 0 (position #\, pref2))))
    (concatenate 'string fname " " lname)))

(defun change-mark-csv (csv mark)
  (let* ((pref1 (subseq csv 0 (position #\, csv :from-end 0)))
	 (pref2 (subseq pref1 0 (position #\, pref1 :from-end 0))))
    (concatenate 'string pref2 "," mark ",#")))

(defun get-insert-grade (log-file-stream stream csv ht)
  (let* ((std-name (get-std-name csv))
	 (v (gethash std-name ht)))
    (if v
        (let ((new-mark (change-mark-csv csv (car (submission-evaluation v)))))
          (format log-file-stream "Mark of student ~a changed from ~a to ==> ~a~%" std-name csv new-mark)
          (format stream "~A~%"  new-mark))
	(progn
          (format log-file-stream "Student name: ~S did not submit assignment!~%" std-name)
          (format t "Student name: ~S did not submit assignment!~%" std-name)))))

(defun generate-marks-spreadsheet (log-file-stream d2l-file folder ht)
  (with-open-file (in d2l-file :direction :input)
    (with-open-file (out (merge-pathnames folder "grades.csv")
			 :direction :output :if-exists :supersede)
      (format out "~A~%" (read-line in nil))
      (loop for line = (read-line in nil)
	    while line do
	      (get-insert-grade log-file-stream out line ht)))))

(defun check-input-files (lf)
  (when lf
    (if (probe-file (car lf))
	(check-input-files (cdr lf))
	(error "Folder/file ~S does not exist." (car lf)))))

(defun cleanup-folder (folder)
  (if  (probe-file folder)
       (sb-ext:delete-directory folder :recursive t)))

(defun replace-char (s c r)
  (setf (aref s (position c s)) r)
  s)

(defun consume-until (s c)
  (subseq s (1+ (position c s))))

(defun check-foldername (p)
  "Adds a / to the end of the folder name if it is not there already"
  (if (char= (aref p (1- (length p))) #\/)
      p
      (concatenate 'string p "/")))

(defun automark (submissions-dir grades-export-file test-cases-dir results due-date-time &optional (weights nil))
  (check-input-files (list submissions-dir grades-export-file test-cases-dir))
  (let* ((is-zipped t)
         (root-folder (check-foldername  (namestring (ensure-directories-exist results :verbose T))))
         (feedback-folder (merge-pathnames "student-feedback/" root-folder))
	 (feedback-zipped (merge-pathnames root-folder "student-feedback.zip"))
	 (unzipped-subs-folder (merge-pathnames "submissions/" root-folder))
	 (subms-folder (progn
                         (cleanup-folder feedback-folder)
                         (cleanup-folder unzipped-subs-folder)
                         (create-folder-ifzipped is-zipped submissions-dir unzipped-subs-folder)))
	 (sfolders (directory (concatenate 'string (namestring subms-folder) "*/")))
	 (h-table (make-hash-table :test 'equal)))
    (with-open-file (log-file-stream (ensure-directories-exist (merge-pathnames "automarker-history/log.txt" (user-homedir-pathname)));(asdf:system-source-directory :automarker) "log.txt")
                                     :direction :output
                                     :if-exists :append
                                     :if-does-not-exist :create)
      (let ((broadcast-stream (make-broadcast-stream *standard-output* log-file-stream)))
        (format broadcast-stream "~a: Started marking assignments~%Solution evaluations:~%" (get-date-time))
        (dolist (folder sfolders)
          (multiple-value-bind (key date) (get-key-and-date folder)
            (let* ((pref (consume-until (consume-until key #\-) #\-)) ;(splice-at-char key #\-))
                   (std-name (subseq pref 1 (1- (length pref))))
                   (student-solution (car (directory (merge-pathnames folder "*.*")))) ; gets first file in the directory
                   (sdate (check-dt (form-date-time (replace-char date #\, #\ ))))
                   (ddate (check-dt due-date-time))
                   (seval (evaluate-solution student-solution ddate sdate test-cases-dir weights)))
              (format log-file-stream "Student *~a*,  result:~%~a~%" std-name seval)
              (setf (gethash std-name h-table) (make-submission :std-name std-name
                                                                :date sdate
                                                                :evaluation seval))
              (generate-std-feedback key seval feedback-folder))))
        (format broadcast-stream "Generating the zipped feedback folder...~%")
        (zip:zip feedback-zipped feedback-folder :if-exists :supersede)
        (format broadcast-stream "Done.~%")
        (format t "Generating the grades spreadsheet...~%")
        (generate-marks-spreadsheet log-file-stream grades-export-file root-folder h-table)
        (format broadcast-stream "Done.~%")
        (format broadcast-stream "~a: Finished marking assignments~%" (get-date-time))))))


(defun quit ()
  (in-package :cl-user))
