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
(require 'dash)

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
(cl-defun extract-matching-strings (regexp &key count startpos endpos noerror)
  "Extract strings from current buffer that match subexpressions of REGEXP.
If COUNT is supplied use the COUNT'th match of REGEXP.
The returned list contains the whole match followed by matches to subexpressions 
of REGEXP (in order).

If STARTPOS is supplied searching starts at that buffer position, otherwise it
starts from the current position. If ENDPOS is supplied the the match must
occur before that position.
By default if no match is found then an error is thrown, unless NOERROR is 
non-nil in which case nil will be returned."
  (if startpos (goto-char startpos))
  (if (re-search-forward regexp endpos noerror count)
      (match-strings-no-properties regexp)))

;;;###autoload
(cl-defun extract-matching-rectangle (tl br &key (inctl t) (incbr t) rows cols noerror idxs)
  "Extract a rectangle of text (list of strings) from the current buffer.
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

If no matching rectangle is found then an error is thrown unless :NOERROR is non-nil.
To return a subset of the rows of the extracted rectangle set the :IDXS argument to
a list of indices of rows to return (0 indicates 1st row), or just a single number
to return a single row."
  ;; check we have the required arguments
  (if (not (and (or tl br)
		(or (and tl br)
		    (and rows cols))))
      (error "Missing :tl/:br or :cols & :rows arguments"))
  ;; find tl & br positions
  (cl-flet* ((search (regex matchfun count) ;function to find position by regexp
		     (if (re-search-forward regex nil t count)
			 (if (match-string 1)
			     (funcall matchfun 1)
			   (funcall matchfun 0))
		       (if noerror 'nomatch
			 (error "Unable to match regex: %s" regex))))
	     (getpos (arg matchfn)
		     (cond
		      ((integerp arg) arg)
		      ((and (numberp arg) (<= arg 1) (>= arg 0))
		       (round (* arg (- (point-max) (point-min)))))
		      ((stringp arg) (search arg matchfn nil))
		      ((and (listp arg)
			    (stringp (car arg))
			    (integerp (cdr arg)))
		       (search (car arg) matchfn (cdr arg)))))
	     (adjust1 (begin type rows cols) ;function to find missing tl/br value, based on rows & cols args
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
			  (point)))))
    (let* ((tl2 (getpos tl (if inctl 'match-beginning 'match-end)))
	   (br2 (getpos br (if incbr 'match-end 'match-beginning)))
	   (strs (unless (memq 'nomatch (list tl2 br2))
		   (cond
		    ((and tl2 br2 (eq cols t))
		     (split-string (buffer-substring-no-properties tl2 br2) "\n"))
		    ((and tl2 br2 (eq rows t))
		     (extract-rectangle (adjust2 tl2 t) (adjust2 br2 nil)))
		    ((and tl2 br2) (extract-rectangle tl2 br2))
		    ((and (or tl2 br2) rows cols)
		     (extract-rectangle (or tl2 (adjust1 br2 t rows cols))
					(or br2 (adjust1 tl2 nil rows cols))))))))
      (cond ((numberp idxs) (-select-by-indices (list idxs) strs))
	    ((and (not (null idxs)) (listp idxs)) (-select-by-indices idxs strs))
	    (t strs)))))

(cl-defun copy-rectangle-to-buffer (tl br &key (inctl t) (incbr t) rows cols)
  "Copy a rectangular region of the current buffer to a new buffer.
Return the new buffer.
The arguments are the same as for `extract-matching-rectangle' apart from 
REPS, NOERROR and JOIN which are not included. If no matching rectangle can be
found an error will be thrown."
  (let ((rect (extract-matching-rectangle
	       tl br :inctl inctl :incbr incbr :rows rows :cols cols :noerror nil))
	(buf (generate-new-buffer " *extracted rectangle*")))
    (with-current-buffer buf
      (insert (mapconcat 'identity rect "\n"))
      (goto-char (point-min)))
    buf))

(defmacro extract-keyword-arg (key lstsym &optional pred)
  "Remove KEY & following item from list referenced by LSTSYM, and return item.
A key here means a symbol whose first character is :
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

(defmacro extract-first-keyword-arg (lstsym &optional pred)
  "Remove & return first key & following item from list referenced by LSTSYM.
A key here means a symbol whose first character is :
LSTSYM should be a symbol whose value is a list.
If there are no keys in the list then return nil, otherwise return a cons cell
whose car is the key and whose cdr is the corresponding value.
If predicate function PRED is supplied then an error will be thrown if
PRED returns nil when supplied with the key value as argument."
  (let ((lst (gensym)))
    `(let* ((,lst (eval ,lstsym))
	    (ind (-find-index (lambda (x)
				(and (symbolp x)
				     (string-match "^:" (symbol-name x)))) ,lst)))
       (unless (not ind)
	 (let ((key (nth ind ,lst))
	       (val (nth (1+ ind) ,lst)))
	   (set ,lstsym (append (-take ind ,lst) (-drop (+ 2 ind) ,lst)))
	   (if (and ,pred (not (funcall ,pred val)))
	       (error "Invalid value for %S" key)
	     (cons key val)))))))

(defmacro loop-over-keyword-args (lst &rest body)
  "Loop over the keyword args in list LST, evaluating BODY forms each time.
For each iteration of the loop, `key' will be bound to the current keyword,
`value' will be bound to the corresponding value, and `keyvaluepair' will
be bound to a cons cell containing these elements (key & value)."
  `(let ((lstsym ,lst))
     (cl-loop for keyvaluepair = (extract-first-keyword-arg 'lstsym)
	      for key = (car keyvaluepair)
	      for value = (cdr keyvaluepair)
	      while keyvaluepair do (eval '(progn ,@body)))))

(defcustom extract-text-wrappers nil
  "A list of wrapper functions that can be used with `extract-text'.
Each element has the form (NAME ARGLIST EXPRESSION [EXPRESSION ...]),
and represents a function which extracts text from the current buffer
and returns it as a string or list of strings.

NAME is a symbol naming the wrapper function.

ARGLIST is a list whose elements have the form (ARGUMENT DEFAULT-VALUE).
These variables are available when evaluating the expressions.

EXPRESSION are elisp forms. They are wrapped in a `progn' and
compose the body of the wrapper function. This body is executed
when the function is called by name --e.g. (wrapper arg1 arg2)-- 
as part of `extract-text' (which see).

Each wrapper function should return a string or list of strings."
  :group 'extract-text
  :type  '(repeat (cons (symbol :tag "Wrapper name")
                        (cons
                         (repeat :tag "Argument list"
                                 (list (symbol :tag "Argument name")
                                       (sexp :tag "Default value")))
                         (repeat (sexp :tag "Expression"))))))

;; TODO: return 'skip from `move' function, and remove all 'skip symbols from
;; returned list.
(cl-defmacro extract-text (&rest args)
  "Extract text from buffer according to specifications in ARGS.
ARGS should be a list/tree of wrapper functions for extracting bits of text (see below).
By default the current buffer is used, but you can specify a different buffer by 
including a :buffer keyword arg in ARGS (as either a buffer or buffer name).

The text extraction is carried out in a sequential and recursive manner according 
to the elements of ARGS. Each element may be either a function for performing a 
specific extraction or cursor reposition (see below), a keyword arg & corresponding 
value (see below), or a list which is then traversed recursively.
This allows for a great amount of flexibility in the extraction specification.

AVAILABLE FUNCTIONS:

The following functions can be used to extract text or move the cursor:

 (regex regexp &key count startpos endpos noerror)
This is a wrapper around `extract-matching-strings' (which see).
Unlike `extract-matching-strings', if regexp contains subexpressions then only matches 
to those subexpressions will be returned, otherwise the whole match is returned.

 (rect tl br &key (inctl t) (incbr t) rows cols noerror)
This is a wrapper around `extract-matching-rectangle' (which see). It takes exactly the
same arguments, but also allows the tl argument to be omitted even when the rows & cols
arguments are omitted, in which case tl is set to the current cursor position.

 (move &rest all &key fwdregex bwdregex fwdchar bwdchar 
      fwdline bwdline fwdword bwdword fwdmark bwdmark pos)
This function is used for repositioning the cursor in between calls to extraction functions.
The keyword args are processed sequentially, each one specifying a cursor movement, and may
contain repeats. The keyword args specify the following movements:

   fwdregex/bwdregex - call `re-search-forward'/`re-search-backward' with fwdregex/bwdregex as 
   argument. If the arg is a list then call the appropriate function with all arguments in the 
   list (in order).
   
   pos             - call `goto-char' with pos as argument
   
   fwdchar/bwdchar - call `forward-char'/`backward-char' with fwdchar/bwdchar as argument.
   
   fwdline/bwdline - call `forward-line'/`backward-line' with fwdline/bwdline as argument.
   
   fwdword/bwdword - call `right-word'/`left-word' with fwdword/bwdword as argument.
   
   fwdline/bwdline - call `forward-line' with fwdline/(- bwdline) as argument.
   
   fwdmark/bwdmark - before each extraction/movement function is called, the current cursor
                     position is added to the end of a list. The fwdmark/bwdmark args tell the
                     move function to move forward from the start or backward from the end of 
                     that list. E.g. \":bwdmark 2\" will move the cursor to the position before
                     the 2nd last function call.

 For example (move :bwdmark 3 :fwdregex \"foo\" :fwdword 2) will first move the cursor to the position
 it was in just before the 3rd previous function call, then move to the next occurrence of \"foo\",
 and then move forward 2 words.

You may also call any of the user functions defined in `extract-text-wrappers'.

KEYWORD ARGS:

The following keyword args may be used to specify how to deal with the extractions in the current
list. 

:REPS    - number of times to repeat the current list of extractions (default 1)
:NOERROR - if set to 'skip then extractions that throw errors will be ignored, if set to any other 
           non-nil symbol then that symbol will be returned in place of any extractions that throw
           errors
:FLATTEN - specify that returned list should be flattened to this depth with `-flatten-n' function 

The following keyword args are passed to `extract-matching-rectangle' (which see) to restrict the
text to a rectangle in the current buffer before performing extractions: 
  :TL, :BR, :INCTL, :INCBR, :ROWS, :COLS
This allows you to narrow down the search space for the extraction functions.

EXAMPLES:

 (extract-text (regex \"[0-9]+\\.[0-9]+\") :REPS 5 :NOERROR 'NA)
 
 explanation: extract the first 5 numbers from the current buffer. 
              If there are fewer than 5 numbers, pad with 'NA.

 (extract-text (regex \"[0-9]+\\.[0-9]+\") :REPS 5 :NOERROR 'NA :buffer \"foobar\")

 explanation: as above but extract from the buffer named \"foobar\".

 A more complex example:

 (extract-text (rect \"Address:\" nil :inctl nil :rows 4 :cols 25)
               (move :bwdmark 1)
               ((regex \"Amount: *\\([0-9]+\\.[0-9]+\\)\") :REPS 3 :TL \"Address:\" :BR \"Total:\" :INCBR nil :COLS t)
               (regex \"Total: *\\([0-9]+\\.[0-9]+\\)\")
               :REPS 1000 :NOERROR 'skip)

 explanation: repeat the following extraction; extract an address in a rectangle of 4 rows and 25 columns, 
                                               move back to a position before the address
                                               extract three \"Amount\" numbers between \"Address\" and \"Total\"
                                               extract a \"Total\" number.
              repeat as many times possible upto 1000 times."
  (let ((args2 args))
    ;; First set the buffer
    `(let* (,@(extract-keyword-bindings 'args2 nil :buffer)
	    (buf (or buffer (current-buffer))))
       ;; scope in some wrapper functions
       (cl-flet* ((regex (regexp &key count startpos endpos noerror)
			 (let ((txt (extract-matching-strings
				     regexp :count count :startpos startpos
				     :endpos endpos :noerror noerror))
			       (fn (if (> (regexp-opt-depth regexp) 0) 'cdr 'identity)))
			   (if (> (regexp-opt-depth regexp) 0) (cdr txt) txt)))
		  (rect (tl br &key (inctl t) (incbr t) rows cols noerror idxs)
			(if (not (or tl (and rows cols))) (setq tl (point)))
			(extract-matching-rectangle
			 tl br :inctl inctl :incbr incbr :rows rows :cols cols :noerror noerror :idxs idxs))
		  (move (&rest all &key fwdregex bwdregex fwdchar bwdchar fwdline
			       bwdline fwdword bwdword fwdmark bwdmark pos)
			(loop-over-keyword-args
			 all (case key
			       (:fwdregex (if (listp value) (apply 're-search-forward value)
					    (re-search-forward value)))
			       (:bwdregex (if (listp value) (apply 're-search-backward value)
					    (re-search-backward value)))
			       (:fwdchar (forward-char value))
			       (:bwdchar (backward-char value))
			       (:fwdline (forward-line value))
			       (:bwdline (forward-line (- value)))
			       (:fwdword (right-word value))
			       (:bwdword (left-word value))
			       (:fwdmark (if (last positions value) ;assumes `positions' list is in scope
					     (goto-char (car (last positions value)))))
			       (:bwdmark (if (nth value positions) ;assumes `positions' list is in scope
					     (goto-char (nth value positions))))
			       (:pos (goto-char value))))
			'skip)
		  ,@(cl-loop for (name . code) in extract-text-wrappers
			     if (> (length code) 1)
			     collect `(,name (,@(car code)) ,@(cdr code))
			     else
			     collect (list name nil code)))
	 (cl-macrolet ((recurse
			(args3)	;do not be tempted to use &rest here, you'll get infinite recursion!
			(if (or (not (listp args3))
				(functionp (car args3))
				(eq (car args3) 'quote)
				(macrop (car args3))
				(memq (car args3)
				      (remove 'nil
					      (list 'regex 'rect 'move (mapcar 'car extract-text-wrappers)))))
			    args3
			  (let ((args4 args3)) ;need this let form so we can use a symbol 'args4 to access the input
			    `(let ,(extract-keyword-bindings
				    'args4 t :REPS :NOERROR :TL :BR :INCTL :INCBR :ROWS :COLS :FLATTEN)
			       ;; set defaults and get buffer containing text
			       (let* ((REPS (or REPS 1))
				      (FLATTEN (or FLATTEN 0))
				      (buf2 (if (not (or TL BR)) (current-buffer)
					      ;; execute the restriction (if any) specified by the TL, BR, etc. 
					      (goto-char (point-min))
					      (copy-rectangle-to-buffer
					       TL BR :inctl INCTL :incbr INCBR :rows ROWS :cols COLS)))
				      allresults positions)
				 ;; check for errors, and ignore them if NOERROR is non-nil
				 (with-current-buffer buf2
				   ;; repeat the extraction for REPS repeats
				   (cl-loop for i from 1 to REPS
					    do (let (results)
						 ,@(cl-loop for spec in args4
							    collect
							    `(setq positions (cons (point) positions)
								   results
								   (remove
								    'skip
								    (cons
								     ;; get args for specifying buffer restriction (if any)
								     (condition-case err
									 (recurse ,spec)
								       (error (if NOERROR NOERROR
										(if (or TL BR) (kill-buffer buf2))
										(signal (car err) (cdr err)))))
								     results))))
						 (unless (null results)
						   (setq allresults
							 (cons (-flatten-n 1 (reverse results)) allresults))))))
				 (if (or TL BR) (kill-buffer buf2))
				 (-flatten-n FLATTEN (reverse allresults))))))))
	   (with-current-buffer buf
	     (save-excursion (goto-char (point-min)) (recurse ,args2))))))))

(defmacro extract-text-from-buffers (buffers &rest spec)
  "Extract text from buffers listed in BUFFERS or matching regexp BUFFERS.
SPEC is the extraction specification to pass to the `extract-text' function.
BUFFERS can be either a list of buffers, a list of buffer names, or a regexp
matching names of buffers to use.
The return value will be a list of lists. Each sublist will be the list returned
by `extract-text' applied to the corresponding buffer.
You can flatten this list using the `-flatten-n' function (which see)."
  (setq buffers (if (stringp buffers)
		    (cl-loop for buf in (buffer-list)
			     for name = (buffer-name buf)
			     when (string-match buffers name)
			     collect name)
		  (cl-loop for buf in buffers
			   if (stringp buf) collect buf
			   else collect (buffer-name buf))))
  `(list
    ,@(cl-loop for buf in buffers
	       collect `(extract-text :buffer ,buf ,@spec))))

(defmacro extract-text-from-files (files &rest spec)
  "Extract text from FILES.
SPEC is the extraction specification to pass to the `extract-text' function.
FILES can be either a list of filepaths or a wildcard pattern matching several
filepaths (see `file-expand-wildcards').
The return value will be a list of lists. Each sublist will be the list returned
by `extract-text' applied to the corresponding file.
You can flatten this list using the `-flatten-n' function (which see)."
  (setq files (cond ((stringp files) (file-expand-wildcards files))
		    ((listp files) files)
		    (t (error "Invalid argument for files"))))
  `(list
    ,@(cl-loop for file in files
	       for bufexists = (find-buffer-visiting file)
	       for buf = (if (file-readable-p file) (find-file-noselect file))
	       if buf collect `(prog1 (extract-text :buffer ,buf ,@spec)
				 (unless ,bufexists (kill-buffer ,buf))))))

(provide 'extract-text)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "extract-text.el" (buffer-name) (buffer-string) "update")

;;; extract-text.el ends here
