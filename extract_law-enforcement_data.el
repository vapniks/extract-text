;; This file contains an example of how to extract data from pdf files using `extract-text-from-files'
;; It extracts state-by-state data on total number of law enforcement employees from pdf files
;; downloaded from the FBI website.
;; There is a lot more data available in these files, but I only need total employees for now.

;; PDF files must first be downloaded from these URLs: 

;; https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/1995/95sec6.pdf
;; https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/1996/96sec6.pdf
;; https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/1997/97sec6.pdf
;; https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/1998/98sec6.pdf
;; https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/1999/99sec6.pdf
;; https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2000/00sec6.pdf
;; https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2001/01sec6.pdf
;; https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2002/02sec6.pdf
;; https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2003/03sec6.pdf
;; https://www2.fbi.gov/ucr/cius_04/documents/CIUS_2004_Section6.pdf

;; data from 2005 onwards is available in .xls which can be extracted more
;; easily without this elisp code and then appended to the .csv file

;; List of files and associated years
(setq lawfilepairs '(("1995" . "95sec6.pdf")
		     ("1996" . "96sec6.pdf")
		     ("1997" . "97sec6.pdf")
		     ("1998" . "98sec6.pdf")
		     ("1999" . "99sec6.pdf")
		     ("2000" . "00sec6.pdf")
		     ("2001" . "01sec6.pdf")
		     ("2002" . "02sec6.pdf")
		     ("2003" . "03sec6.pdf")
		     ("2004" . "CIUS_2004_Section6.pdf")))

;; Convert the pdf files to text files
(dolist (filepair lawfilepairs)
  (shell-command (concat "/usr/bin/env pdftotext -layout " (cdr filepair))))

;; NOTE: you must edit the text files to make sure that the sections containing the state by state
;; employee figures are arranged correctly so that the following code will match all the data.
;; For each .txt file you will need to move a few lines around (it won't take long):
;; 1) first search for the start of the data: (re-search-forward "ALABAMA:? [0-9]+ agencies;")
;; 2) make sure that each state name is directly above the corresponding "population" line
;;    you will need to change the "DISTRICT OF COLUMBIA" bit to make sure it's all on the same level
;; 3) most of the data will be in 2 columns so that 2 states are named on a single line, with 2 "population" figures below,
;;    or they might be staggered by one line, or they might be on separate lines. In any case you must ensure that the
;;    2nd number following "population" (i.e. the total employees figure) within 2 lines below the corresponding state name
;;    and within 55 chars to the right. Also "population" must start on the line below and no further to the left than the
;;    state name. You should check the figures with the pdf files to make sure they match up on any changes you made.
;; 4) You can check it is correct by evaluating the following code in the buffer containing the text file:
;; (length (extract-text 
;; 	 ((regex "\\([A-Z ]+\\):? [0-9]+ *agencies;")
;; 	  (regex "[Pp]opulation [0-9,]+:?[ .]+\\([0-9,]+\\)")
;; 	  :TL "[A-Z][A-Z ]*:? [0-9]+ *agencies;" :COLS 55 :ROWS 3)
;; 	 :REPS 52 :ERROR 'stop :FLATTEN 1))
;;
;;    It should return 51. If you get a number smaller than that then it has missed some data, and you may need to
;;    make some adjustments.


;; Extract the data
(extract-text-from-files
 (mapcar (lambda (x) (replace-regexp-in-string "\\.pdf" ".txt" (cdr x))) lawfilepairs)
 '(( ;; match the state name
    (regex "\\([A-Z ]+\\):? [0-9]+ *agencies;")
    ;; get year associated with the current file
    (car (rassoc (replace-regexp-in-string "\\.txt" ".pdf" name) lawfilepairs))
    ;; match total number of law enforcement employees
    (regex "[Pp]opulation [0-9,]+:?[ .]+\\([0-9,]+\\)")
    ;; restrict each matching pair to the rectangle defined by the parameters below
    :TL "[A-Z][A-Z ]*:? [0-9]+ *agencies;" :COLS 55 :ROWS 3)
   ;; maximum of 52 repeats
   :REPS 52
   ;; stop when no more matches can be found
   :ERROR 'stop
   ;; flatten the results by 1 level
   :FLATTEN 1)
 ;; join the results together and add a header line
 (lambda (res) (nconc '(("State" "Year" "Employees"))
		      (-flatten-1 res)))
 ;; save as a .csv file
 "US_law-enforcement_employees_1995-2004.csv")

