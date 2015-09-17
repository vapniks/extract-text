;;; extract-text.el --- Functions for extracting/scraping text from buffers

;; Filename: extract-text.el
;; Description: Functions for extracting/scraping text from buffers
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-09-09 12:30:53
;; Version: 0.1
;; Last-Updated: 2015-09-09 12:30:53
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/extract-text
;; Keywords: extensions
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires: ((dash "20150829.433") (macro-utils "1.0"))
;;
;; Features that might be required by this library:
;;
;; cl, dash, macro-utils
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1FgnGFwRES9MGzoieRDWLmLkjqMT3AsjQF
;;
;; This library provides functions for programmatically extracting text from buffers.
;;
;;
;;;;


;;; Installation:
;;
;; Put extract-text.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'extract-text)

;;; Require
(eval-when-compile (require 'cl))
(require 'macro-utils)

;;; Code:

;;;###autoload
(defun match-strings (regexp &optional str)
  "Return a list of all strings matched by last search.
The first element of the list will be the whole match,
and subsequent elements will be matches to non-shy subexpressions
in the matching regexp.
REGEXP should be the regexp that was used in the search,
and if the last search was by `string-match' STR should
be the string searched."
  (cl-loop for i to (regexp-opt-depth regexp)
	   collect (match-string i str)))

;;;###autoload
(defun match-strings-no-properties (regexp &optional str)
  "Return a list of all strings matched by last search.
The first element of the list will be the whole match,
and subsequent elements will be matches to non-shy subexpressions
in the matching regexp.
REGEXP should be the regexp that was used in the search,
and if the last search was by `string-match' STR should
be the string searched."
  (cl-loop for i to (regexp-opt-depth regexp)
	   collect (match-string-no-properties i str)))

;;;###autoload
(cl-defun extract-matching-strings (regexp &key startpos endpos noerror)
  "Extract strings from current buffer that match subexpressions of REGEXP.
If STARTPOS is supplied searching starts at that buffer position, otherwise it
starts from the current position. If ENDPOS is supplied then any matches must
occur before that position.
By default if no match is found then an error is thrown, unless NOERROR is 
non-nil in which case nil is returned.
The return value is a list whose first element is the entire match, and whose 
subsequent elements are the matches to subexpressions of REGEXP (in order).
If any subexpression doesn't match then nil will be returned for that element."
  (if startpos (goto-char startpos))
  (if (re-search-forward regexp endpos noerror)
      (match-strings-no-properties regexp)))

;;;###autoload
(cl-defun extract-matching-rectangles (tl br &key (inctl t) (incbr t) rows cols (reps 1) noerror join)
  "Extract rectangle(s) of text from the current buffer.
The return value is a list of strings (the lines of the rectangle), or a list of such lists if REPS is > 1. 
If JOIN is non-nil then the lines of each rectangle will be joined together with JOIN as a separator,
 and returned as single strings.

The rectangle can be specified in several different ways:

 1) By passing corner positions to TL and BR (see `extract-rectangle')

 2) By passing numbers between 0 & 1 to TL and BR indicating corner positions as a 
    fraction of the buffer size, e.g. 0.5 represents the midpoint of the buffer

 2) By passing regular expressions to TL and BR which match strings delimiting the
    corner positions. If the regexp contains any non-shy subexpressions, the first one will 
    be used for determining the tl/br match.
    By default the matched strings will be included in the rectangle. To exclude the matched 
    tl/br text so the rectangle starts/ends after/before the matched text you must set 
    INCTL and/or INCBR to nil (they are t by default).

 3) By passing cons cells in the form (regex . rep) to TL and BR. In this case the
    rep'th match to regex will be used for the start/end position.

 4) By a mixture of the above methods.

 5) By specifying TL & BR using one of the above methods and setting COLS to t
    in which case all columns of all lines between TL & BR will be included.
    The return value will be all text between TL & BR split at newlines and
    put into a list. Note: this is not necessarily a rectangle exactly.

 6) By specifying TL & BR using one of the above methods and setting ROWS to t
    in which case all rows of all columns between TL & BR will be included.

 5) By specifying just one of TL or BR (as a position, regexp, or cons cell), 
    and also specifying the number of COLS and ROWS of the rectangle. 
    If COLS or ROWS is negative then the columns/rows are counted in the opposite 
    direction to normal, i.e. backwards from TL position or forwards from BR 
    position. This allows you to specify the rectangle from any corner position.

When REPS > 1 rectangles are extracted sequentially (using the same parameters), 
with the search for the next rectangle starting from the end of the previous rectangle.
This only makes sense when TL and/or BR are regexp's as otherwise all the rectangles
will be identical.
If no matching rectangle is found then an error is thrown unless :NOERROR is non-nil 
in which case the rectangles found so far are returned (if none were found then nil is
returned)."
  ;; check we have the required arguments
  (if (not (and (or tl br)
		(or (and tl br)
		    (and rows cols))))
      (error "Missing :tl/:br or :cols & :rows arguments"))
  ;; find tl & br positions
  (cl-flet* ((search (regex matchfun count) ;function to find position by regexp
		     (re-search-forward regex nil nil count)
		     (if (match-string 1)
			 (funcall matchfun 1)
		       (funcall matchfun 0)))
	     (getpos (arg matchfn) ;function to get position given tl/br arg
		     (cond     ;and match-beginning/match-end function
		      ((integerp arg) arg)
		      ((and (numberp arg) (<= arg 1) (>= arg 0))
		       (round (* arg (- (point-max) (point-min)))))
		      ((stringp arg) (search arg matchfn nil))
		      ((and (listp arg)
			    (stringp (car arg))
			    (integerp (cdr arg)))
		       (search (car arg) matchfn (cdr arg)))))
	     (adjust1 (begin type) ;function to find missing tl/br value, based on rows & cols args
		      (save-excursion ;BEGIN is position of non-missing end (br/tl)
			(goto-char begin) ;TYPE is t if we have br but need tl and nil if we have tl but need br
			(let ((col (current-column)))
			  (forward-line (if type (- 1 rows) (- rows 1)))
			  (move-to-column (if type (- col (1- cols))
					    (+ col (1- cols))) t)
			  (point))))
	     (adjust2 (pos type) ;function finds position in same column as POS, but in first/last row
		      (save-excursion ;depending on whether TYPE is non-nil/nil
			(goto-char pos)	;(used when ROW arg is t to get rectangle spanning all rows)
			(let ((col (current-column)))
			  (goto-char (if type (point-min) (point-max)))
			  (move-to-column col t)
			  (point))))
	     (getrect (tl2 br2)	;function to get the rectangle given start and end positions
		      (cond
		       ((and tl2 br2 (eq cols t))
			(split-string (buffer-substring-no-properties tl2 br2) "\n"))
		       ((and tl2 br2 (eq rows t))
			(extract-rectangle (adjust2 tl2 t) (adjust2 br2 nil)))
		       ((and tl2 br2) (extract-rectangle tl2 br2))
		       ((and (or tl2 br2) rows cols)
			(extract-rectangle (or tl2 (adjust1 br2 t))
					   (or br2 (adjust1 tl2 nil)))))))
    (let (results)
      (condition-case err
	  (dotimes (i reps)
	    (let ((tl2 (getpos tl (if inctl 'match-beginning 'match-end)))
		  (br2 (getpos br (if incbr 'match-end 'match-beginning))))
	      (unless (memq 'nomatch (list tl2 br2))
		(let ((txt (getrect tl2 br2)))
		  (setq results (cons (if join (mapconcat 'identity txt join) txt)
				      results))))))
	(error (unless noerror (signal (car err) (cdr err)))))
      (if (> reps 1) (reverse results) (car results)))))

(cl-defun copy-rectangle-to-buffer (tl br &key (inctl t) (incbr t) rows cols)
  "Copy a rectangular region of the current buffer to a new buffer.
Return the new buffer.
The arguments are the same as for `extract-matching-rectangles' apart from 
REPS, NOERROR and JOIN which are not included. If no matching rectangle can be
found an error will be thrown."
  (let ((rect (extract-matching-rectangles
	       tl br :inctl inctl :incbr incbr :rows rows
	       :cols cols :reps 1 :noerror nil :join nil))
	(buf (generate-new-buffer " *extracted rectangle*")))
    (with-current-buffer buf
      (insert (mapconcat 'identity rect "\n"))
      (goto-char (point-min)))
    buf))

(defmacro extract-keyword-arg (key lstsym &optional pred)
  "Remove KEY & following item from list referenced by LSTSYM, and return item.
LSTSYM should be a symbol whose value is a list.
If KEY is not in the list then return nil.
If predicate function PRED is supplied then an error will be thrown if
PRED returns nil when supplied with the key value as argument."
  (let ((lst (gensym)))
    `(let* ((,lst (eval ,lstsym))
	    (ind (-elem-index ,key ,lst)))
       (unless (not ind)
	 (let ((val (nth (1+ ind) ,lst)))
	   (set ,lstsym (append (-take ind ,lst) (-drop (+ 2 ind) ,lst)))
	   (if (and ,pred (not (funcall ,pred val)))
	       (error "Invalid value for %S" ,key)
	     val))))))
 
;; plan call above functions after copying required rectangle into separate buffer
;; limits of rectangle are defined by regexp/position/percentage args top bottom left right
;; or maybe just min & max args to main function. 
;; Better to do it this way (copying rectangle to a temp buffer), since otherwise we have
;; problems when the regexp matches partly inside the required box and partly outside

;; how to specify repetition until no match?? Use t value for :rep arg
;; allow :join or :sep arg to specify whether output from specs in list should be joined into single list
;; or returned separate lists (within main returned list)
;(extract-text (rect a b) ((rect a b :tl :br) (regex "foo") :TL "blah" :BR "foo"))

(defmacro extract-keyword-bindings (args &optional check &rest keys)
  "Extract KEYS and corresponding values from ARGS, and return in let-style bindings list.
If ARGS is a symbol referring to a list, then KEYS and corresponding values will be removed from ARGS.
If CHECK is non-nil then if there are any keys (beginning with :) in ARGS other than those in KEYS 
an error will be thrown."
  (let ((args2 (gensym))
	(args3 (gensym)))
    `(let ((,args2 ,args)
	   (,args3 (if (symbolp ,args) ,args ',args2)))
       (if ,check
	   (let* ((allkeys (-filter (lambda (x) (and (symbolp x)
						     (string-match "^:" (symbol-name x))))
				    (eval ,args3)))
		  (unusedkeys (-difference allkeys ',keys)))
	     (if unusedkeys
		 (error "Keyword argument %s not one of %s" (car unusedkeys) ',keys))))
       (cl-loop for key in ',keys
		collect (list (if (string-match "^:" (symbol-name key))
				  (intern (substring (symbol-name key) 1))
				key)
			      (extract-keyword-arg key ,args3))))))

(defcustom extract-text-saved-wrappers nil
  "A list of wrapper functions that can be used with `extract-text'.
Each element has the form (NAME ARGLIST EXPRESSION [EXPRESSION ...]),
and represents a function which extracts text from the current buffer
and returns it as a string or list of strings.

NAME is a symbol naming the wrapper function.

ARGLIST is a list whose elements have the form (ARGUMENT DEFAULT-VALUE).
These variables are available when evaluating the expressions.

EXPRESSION are elisp forms. They are wrapped in a `progn' and
compose the body of the wrapper function. This body is executed
when the function is called by name --e.g. (wrapper)-- as part of
`extract-text' (which see).

Each wrapper function should return a string or list of strings."
  :group 'extract-text
  :type  '(repeat (cons (symbol :tag "Wrapper name")
                        (cons
                         (repeat :tag "Argument list"
                                 (list (symbol :tag "Argument name")
                                       (sexp :tag "Default value")))
                         (repeat (sexp :tag "Expression"))))))

(defcustom extract-text-max-reps 1000
  "Maximum number of repetitions allowed when :REP arg is set to t in `extract-text' function."
  :group 'extract-text
  :type 'integer)

(cl-defmacro extract-text (&rest args)
  "Extract text from :BUFFER or :STRING according to specifications in ARGS.
If no :BUFFER or :STRING argument is supplied then the current buffer is used.
SPECS should be a list of wrapper functions for extracting bits of text."
  ;; First set the string/buffer args
  (let ((args2 args))
    `(let* (,@(extract-keyword-bindings 'args2 nil :string :buffer)
	    (buf (or buffer
		     (and string (generate-new-buffer "*extract from string*"))
		     (current-buffer)))
	    allresults)
       (if string (with-current-buffer buf (insert string)))
       ;; scope in some wrapper functions
       (cl-flet* ((regex (regexp &key startpos endpos noerror)
			 (let ((txt (extract-matching-strings
				     regexp :startpos startpos :endpos endpos :noerror noerror)))
			   (if (> (length txt) 1) (cdr txt) txt)))
		  (rect (tl br &key (inctl t) (incbr t) rows cols noerror join)
			(extract-matching-rectangles
			 tl br :inctl inctl :incbr incbr :rows rows
			 :cols cols :noerror noerror :join join))
		  ,@(cl-loop for (name . code) in extract-text-saved-wrappers
			     if (> (length code) 1)
			     collect `(,name (,@(car code)) ,@(cdr code))
			     else
			     collect (list name nil code)))
	 ;; loop over the different extraction specifications
	 ,@(cl-loop for spec in args2
		    ;; get args for specifying buffer restriction (if any)
		    collect `(let ,(if (listp (car spec))
				       ;; check for invalid keyword args when specification is a list of functions
				       (extract-keyword-bindings 'spec t :REPS :TL :BR :INCTL :INCBR :ROWS :COLS)
				     (extract-keyword-bindings 'spec nil :REPS :TL :BR :INCTL :INCBR :ROWS :COLS))
			       ;; set defaults and get buffer containing text
			       (let* ((untilerror (eq REPS t))
				      (REPS (if untilerror extract-text-max-reps
					      (or REPS 1)))
				      (buf2 (if (or TL BR)
						;; execute the restriction (if any) specified by the TL, BR, etc. 
						(with-current-buffer buf
						  (goto-char (point-min))
						  (copy-rectangle-to-buffer
						   TL BR :inctl INCTL :incbr INCBR :rows ROWS :cols COLS))
					      buf))
				      results)
				 (condition-case err
				     (with-current-buffer buf2
				       (goto-char (point-min))
				       ;; extract the text into results
				       ;; repeat the extraction for REPS repeats
				       (dotimes (i REPS results)
					 ,@(if (listp (car spec))
					      ;; if we have a list of functions apply them in turn
					      (cl-loop for func in spec
						       collect `(setq results (append results ,func)))
					    ;; otherwise just apply a single function
					     `((setq results (append results ,spec))))))
				   (error (if (or TL BR) (kill-buffer buf2))
					  (unless untilerror
					    (signal (car err) (cdr err)))))
				 (if (or TL BR) (kill-buffer buf2))
				 (setq allresults (cons results allresults))))))
       (reverse allresults))))
	       
(provide 'extract-text)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "extract-text.el" (buffer-name) (buffer-string) "update")

;;; extract-text.el ends here
