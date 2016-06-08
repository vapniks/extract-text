;;; extract-text.el --- Functions for extracting/scraping text from buffers

;; Filename: extract-text.el
;; Description: Functions for extracting/scraping text from buffers
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-09-09 12:30:53
;; Version: 0.2
;; Last-Updated: 2016-05-27 16:30:53
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/extract-text
;; Keywords: extensions
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires: ((dash "20150829.433") (macro-utils "1.0") (keyword-arg-macros "20151006") (ido-choose-function "20151021"))
;;
;; Features that might be required by this library:
;;
;; cl, dash, macro-utils, keyword-arg-macros, ido-choose-function
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
;; The `extract-matching-strings' and `extract-matching-rectangles' macros allow you to perform
;; simple extractions, but for more complex tasks there is `extract-text'.
;; This macro allows complex text extraction specifications using a kind of dsl for text extraction.
;; You can use included wrapper functions for picking out individual bits of text, or define your own
;; and save them in `extract-text-user-wrappers' and `extract-text-user-progs'.
;; You can specify how many times to repeat each individual extraction, how to handle errors or missing values,
;; restrict the buffer for finding certain extractions, how to structure &/or transform the output, etc.
;; See the docstring for `extract-text' for more details.
;; 
;;;;

;;; Commands:
;;
;; `extract-text-from-files' : Extract text from FILES, and save to file or `kill-ring', or insert as org-table.
;; `extract-text-from-buffers' : Extract text from BUFFERS and save to file or `kill-ring', or insert as org-table.
;; `extract-text-from-current-buffer' : Extract text from the current buffer, or active region.
;;
;;; Functions:
;;
;; `match-strings' : Return a list of all strings matched by last search.
;; `match-strings-no-properties' : Return a list of all strings matched by last search without properties
;; `extract-matching-strings' : Extract strings from current buffer that match subexpressions of REGEXP.
;; `extract-matching-rectangle' : Extract a rectangle of text (list of strings) from the current buffer.
;; `copy-rectangle-to-buffer' : Copy a rectangular region of the current buffer to a new buffer.
;; `extract-text-choose-prog' : Choose item from `extract-text-user-progs', and return arguments for `extract-text'.
;; `extract-text-compile-prog' : Create compiled version of `extract-text' with arguments PRG applied.
;;
;;; Macros:
;;
;; `extract-text' : Extract text from current buffer according to specifications in ARGS.
;;
;;; Options: 
;;
;; `extract-text-user-wrappers' : A list of wrapper functions that can be used with `extract-text'.
;; `extract-text-user-progs' : A list of different extraction programs/specifications

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
(require 'keyword-arg-macros)
(require 'ido-choose-function)

;;; TODO: have a look at these tools: http://machinelearning.inginf.units.it/data-and-tools

;;; Code:

;;;###autoload
(defcustom extract-text-user-wrappers nil
  "A list of wrapper functions that can be used with `extract-text'.
These functions complement the ones defined in `extract-text-builtin-wrappers'.

Each element has the form (NAME ARGLIST EXPRESSION [EXPRESSION ...]),
and represents a function which extracts text from the current buffer
and returns it as a string or list of strings.

NAME is a symbol naming the wrapper function.

ARGLIST is a list of function arguments, and should be parenthesised.
Full common Lisp arguments specifications are allowed, e.g.
 (arg1 arg2 &optional (arg3 defaultval) &key foo baa)

EXPRESSION are elisp forms. They are wrapped in a `progn' and
compose the body of the wrapper function. This body is executed
when the function is called by name --e.g. (wrapper arg1 arg2)--
as part of `extract-text' (which see).

Each wrapper function should return a string or list of strings,
and may make use of the functions in `extract-text-builtin-wrappers',
and also the DEBUG variable which indicates whether `extract-text'
was called with a non-nil :DEBUG arg."
  :group 'extract-text
  :type  '(repeat (cons (symbol :tag "Name")
                        (cons
			 (restricted-sexp :match-alternatives (listp) :tag "Arglist")
			 (repeat (sexp :tag "Expression"))))))

;; TODO: DEBUG for move function (also fix debug for `extract-matching-rectangle')
;;;###autoload
(defvar extract-text-builtin-wrappers
  '((regex (regexp &key count startpos endpos (error t))
	   (let ((txt (extract-matching-strings
		       regexp :count count :startpos startpos
		       :endpos endpos :error error :debug DEBUG)))
	     (if (listp txt)
		 (if (> (regexp-opt-depth regexp) 0) (cdr txt) txt)
	       txt)))
    (rect (tl br &rest restkeys &key (inctl t) (incbr t) rows cols (error t) idxs)
	  (if (not (or tl (and rows cols))) (setq tl (point)))
	  (apply 'extract-matching-rectangle tl br :debug DEBUG restkeys))
    (move (&rest all &key fwdregex bwdregex fwdchar bwdchar fwdline bwdline
		 fwdword bwdword fwdmark bwdmark pos col row rowend colend)
	  (cl-flet ((debugit (fmtstr)
			     (let ((cmdstr (concat "(move " (substring (format "%s" all) 1 -1) ")")))
			       (save-match-data
				 (extract-text-debug-next
				  nil cmdstr (list (cons (string-match (format fmtstr key) cmdstr)
							 (match-end 0))))))))
	    (loop-over-keyword-args
	     all (case key
		   (:fwdregex (if (listp value) (apply 're-search-forward value)
				(re-search-forward value))
			      (if DEBUG (debugit "%s \"[^\"]*\"")))
		   (:bwdregex (if (listp value) (apply 're-search-backward value)
				(re-search-backward value))
			      (if DEBUG (debugit "%s \"[^\"]*\"")))
		   (:fwdchar (forward-char value)
			     (if DEBUG (debugit "%s [0-9]*")))
		   (:bwdchar (backward-char value)
			     (if DEBUG (debugit "%s [0-9]*")))
		   (:fwdline (forward-line value)
			     (if DEBUG (debugit "%s [0-9]*")))
		   (:bwdline (forward-line (- value))
			     (if DEBUG (debugit "%s [0-9]*")))
		   (:fwdword (right-word value)
			     (if DEBUG (debugit "%s [0-9]*")))
		   (:bwdword (left-word value)
			     (if DEBUG (debugit "%s [0-9]*")))
		   (:fwdmark (if (last positions value) ;assumes `positions' list is in scope
				 (goto-char (car (last positions value))))
			     (if DEBUG (debugit "%s [0-9]*")))
		   (:bwdmark (if (nth value positions) ;assumes `positions' list is in scope
				 (goto-char (nth value positions)))
			     (if DEBUG (debugit "%s [0-9]*")))
		   (:pos (goto-char value)
			 (if DEBUG (debugit "%s [0-9]*")))
		   (:col (move-to-column value t)
			 (if DEBUG (debugit "%s [0-9]*")))
		   (:row (let ((col (current-column)))
			   (goto-char (point-min))
			   (forward-line (1- value))
			   (move-to-column col t))
			 (if DEBUG (debugit "%s [0-9]*")))
		   (:rowend (end-of-line)
			    (if DEBUG (debugit "%s [0-9]*")))
		   (:colend (let ((col (current-column)))
			      (goto-char (point-max))
			      (move-to-column col t))
			    (if DEBUG (debugit "%s [0-9]*"))))))
	  'skip)
    (transform (regexp rep strs &key fixedcase literal subexp start idxs)
	       (let ((strs (if (stringp strs) (list strs) strs))
		     (idxs (if (listp idxs) idxs (list idxs))))
		 (-map-indexed (lambda (idx str)
				 (if (or (not idxs)
					 (memq idx idxs))
				     (replace-regexp-in-string
				      regexp rep str fixedcase literal subexp start)
				   str)) strs))))
  "A list of builtin wrapper functions that can be used with `extract-text':

  (regex regexp &key count startpos endpos error)
  This is a wrapper around `extract-matching-strings' (which see).
  Unlike `extract-matching-strings', if regexp contains subexpressions then only matches 
  to those subexpressions will be returned, otherwise the whole match is returned.
  Note: to ensure that a 'nil symbol is returned in the results on error set the error arg to ''nil 
  not 'nil or nil

  (rect tl br &key (inctl t) (incbr t) rows cols error)
  This is a wrapper around `extract-matching-rectangle' (which see). It takes exactly the
  same arguments, but also allows the tl argument to be omitted even when the rows & cols
  arguments are omitted, in which case tl is set to the current cursor position.
  Note: to ensure that a 'nil symbol is returned in the results on error set the error arg to ''nil 
  not 'nil or nil

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

  row/col         - move to a particular row/column, while staying in the same column/row that
  you started in. Extra whitespace will be added to the row if necessary to 
  ensure that the required column exists (see `move-to-column').

  rowend/colend   - move to the end of the current row/column.   

  For example (move :bwdmark 3 :fwdregex \"foo\" :fwdword 2) will first move the cursor to the position
  it was in just before the 3rd previous function call, then move to the next occurrence of \"foo\",
  and then move forward 2 words.

  (transform (regexp rep strs &key fixedcase literal subexp start idxs)
	     This function is a wrapper around `replace-regexp-in-string' for transforming extracted strings.
	     The STRS argument can be a call to an extraction function that returns a string or list of strings.
	     The keyword argument IDXS can be a single index or a list of indexes indicating which elements of
	     STRS should be transformed. The other arguments are the same as for `replace-regexp-in-string'.
	     Examples: 
	     (transform \"this\" \"that\" (rect \"foo\" \"baa\") :idxs '(1 4))

	     explanation: replace \"this\" with \"that\" in the 2nd and 5th strings of the rectangle between \"foo\" and \"baa\"

	     (transform \"\\\\([0-9]+\\\\)/\\\\([0-9]+\\\\)/\\\\([0-9]+\\\\)\" \"[\\\\3-\\\\2-\\\\1]\"
			(regex \"\\\\([0-9]+\\\\)/\\\\([0-9]+\\\\)/\\\\([0-9]+\\\\)\"))

	     explanation: extract a date in the form DD/MM/YYYY and convert to an org-timestamp
	     (you may want to add something like this to `extract-text-user-wrappers')")

;;;###autoload
(defcustom extract-text-user-progs nil
  "Lists of arguments to use with `extract-text'.
This list can be used to store useful extraction programs that you might want to reuse.
Each element is a list of three items:
 1) A name or short description of the extraction.
 2) A list of arguments for `extract-text', or an interactive function which returns 
    such a list. 
 3) An optional postprocessing function to rearrange the list of results returned by
    `extract-text-from-buffers' and `extract-text-from-files' before they are exported
    to an org-table or file (e.g. concatenate the results with `-flatten-1' or extract
    column headers with `key-values-to-lists', or add a row of headers). 
    The function should take the list of results to be processed as its only argument, 
    and return the rearranged list.

The function `extract-text-choose-prog' can be used to prompt the user for
one of these programs and its arguments (in the case of interactive functions)."
  :group 'extract-text
  :type '(repeat (list (string :tag "Name or short description")
		       (choice (sexp :tag "List of arguments" :value (nil :REPS 1))
			       (restricted-sexp :match-alternatives (commandp)
						:tag "Command"))
		       (function :tag "Postprocessing function" :value "identity"))))

(defvar extract-text-old-overlays nil
  "List of overlays highlighting previous matches while debugging.")

(defvar extract-text-overlays nil
  "List of overlays highlighting current matches while debugging.")

(defcustom extract-text-reset-debug-highlights nil
  "If non-nil then debug highlighting will be reset on each new repetition."
  :group 'extract-text
  :type 'boolean)

(defun extract-text-propertize-string (str start end &rest props)
  "Apply PROPS to STR between positions START and END."
  (concat (substring str 0 start)
	  (apply 'propertize (substring str start end) props)
	  (substring str end)))

(defun extract-text-dehighlight (&optional currentonly)
  "Remove debug highlighting.
Removes all overlays in `extract-text-old-overlays' and `extract-text-overlays'.
If optional arg CURRENTONLY is non-nil, only remove `extract-text-overlays'."
  (while extract-text-overlays
    (delete-overlay (car extract-text-overlays))
    (setq extract-text-overlays
	  (cdr extract-text-overlays)))
  (unless currentonly
    (while extract-text-old-overlays
      (delete-overlay (car extract-text-old-overlays))
      (setq extract-text-old-overlays
	    (cdr extract-text-old-overlays)))))

(defun extract-text-debug-next (regions &optional msg msgregions)
  "Prompt user to step forward through debugging.
REGIONS should be a list of cons cells each of which contains a pair (start . end)
of positions delimiting regions to be highlighted. If REGIONS is nil then no changes
to highlighting will be applied.
Optional MSG is a string to prepend to the user prompt, and MSGREGIONS is an optional
list of cons cells indicating regions of MSG that should be highlighted"
  (when regions
    (while extract-text-overlays
      (let* ((ov (car extract-text-overlays))
	     (ovstart (overlay-start ov))
	     (ovend (overlay-end ov)))
	(when (and ovstart ovend)
	  (let ((ov2 (make-overlay ovstart ovend)))
	    (overlay-put ov2 'priority 1000)
	    (overlay-put ov2 'face lazy-highlight-face)
	    (overlay-put ov2 'window (selected-window))
	    (setq extract-text-old-overlays (cons ov2 extract-text-old-overlays))))
	(delete-overlay ov)
	(setq extract-text-overlays
	      (cdr extract-text-overlays))))
    (cl-loop for (beg . end) in regions
	     do (let ((ov2 (make-overlay beg end)))
		  (overlay-put ov2 'priority 1001)
		  (overlay-put ov2 'face isearch-face)
		  (setq extract-text-overlays
			(cons ov2 extract-text-overlays)))))
  (let ((inhibit-quit t))
    (cl-loop for (beg . end) in msgregions
	     do (setq msg (extract-text-propertize-string
			   msg beg end 'face isearch-face)))
    (when (eq (read-char (format "%s\nPress any key to continue, or C-g to quit" msg))
	      7)
      (extract-text-dehighlight)
      (setq inhibit-quit nil)
      (keyboard-quit))))

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
(cl-defun extract-matching-strings (regexp &key count startpos endpos (error t) debug)
  "Extract strings from current buffer that match subexpressions of REGEXP.
If COUNT is supplied use the COUNT'th match of REGEXP.
The returned list contains the whole match followed by matches to subexpressions 
of REGEXP (in order).

If STARTPOS is supplied searching starts at that buffer position, otherwise it
starts from the current position. If ENDPOS is supplied then the match must
occur before that position.
By default if no match is found then an error is thrown. If ERROR is set to anything
other than t (including nil) then that value will be returned if there is an error.

If DEBUG is non-nil then matches will be highlighted and the user will be prompted
to continue after each match."
  (if startpos (goto-char startpos))
  (let (matches)
    (if (condition-case err
	    (re-search-forward regexp endpos (not error) count)
	  (error (cond ((eq error t)
			(signal (car err) (cdr err)))
		       (t nil))))
	(setq matches (match-strings-no-properties regexp))
      error)
    (when debug
      (let ((strpos1 -1)
	    (strpos2 0)
	    (rlen (length regexp))
	    startpos2 endpos2)
	(if (> (length matches) 1)
	    (cl-loop for matchnum from 1 to (1- (length matches))
		     do (progn
			  (setq strpos1 (string-match-p "\\\\(" regexp (min (1+ strpos1) rlen))
				strpos2 (+ 2 (string-match-p "\\\\)" regexp (min strpos2 rlen))))
			  (extract-text-debug-next
			   (list (cons (match-beginning matchnum)
				       (match-end matchnum)))
			   (format "(regex \"%s\")" regexp)
			   (list (cons (+ strpos1 8) (+ strpos2 8))))))
	  (extract-text-debug-next
	   (list (cons (match-beginning 0) (match-end 0)))
	   (format "(regex \"%s\")" regexp) (list (cons 8 (+ (length regexp) 8)))))))
    matches))

;;;###autoload
(cl-defun extract-matching-rectangle (tl br &key (inctl t inctlp) (incbr t incbrp)
					 (rows nil rowsp) (cols nil colsp) (error t) (idxs nil idxsp) debug)
  "Extract a rectangle of text (list of strings) from the current buffer.
The rectangle can be specified in several different ways:

 1) By passing corner positions to TL and BR (see `extract-rectangle')

 2) By passing numbers between 0 & 1 to TL and BR indicating corner positions as a 
    fraction of the buffer size, e.g. 0.5 represents the midpoint of the buffer

 3) By passing regular expressions to TL and BR which match strings delimiting the
    corner positions. If the regexp contains any non-shy subexpressions, the first one will 
    be used for determining the tl/br match.
    By default the matched strings will be included in the rectangle. To exclude the matched 
    tl/br text so the rectangle starts/ends after/before the matched text you must set 
    INCTL and/or INCBR to nil (they are t by default).

 4) By passing cons cells in the form (regex . rep) to TL and BR. In this case the
    rep'th match to regex will be used for the start/end position.

 5) By a mixture of the above methods.

 6) By specifying just one of TL or BR (as a position, regexp, or cons cell), 
    and also specifying the number of COLS and ROWS of the rectangle. 
    If COLS or ROWS is negative then the columns/rows are counted in the opposite 
    direction to normal, i.e. backwards from TL position or forwards from BR 
    position. This allows you to specify the rectangle from any corner position.

 7) By specifying TL & BR using one of the above methods and setting COLS to t
    in which case all columns of all lines between TL & BR will be included.
    If one of TL/BR is missing it will be inferred to be in the same column as the
    other, but ROWS rows apart. The return value will be all text between TL & BR 
    split at newlines and put into a list. Note: this is not necessarily a rectangle 
    exactly.

 8) By specifying TL & BR using one of the above methods and setting ROWS to t
    in which case all rows of all columns between TL & BR will be included.
    If one of TL/BR is missing it will be inferred to be in the same row as the
    other, but COLS cols apart.

If no matching rectangle is found then an error is thrown unless :ERROR is set to
some other value than t (including nil) in which case that value will be returned instead.
To return a subset of the rows of the extracted rectangle set the :IDXS argument to
a list of indices of rows to return (0 indicates 1st row), or just a single number
to return a single row.

If DEBUG is non-nil then matches will be highlighted and the user will be prompted
to continue after each match."
  ;; check we have the required arguments
  (if (not (and (or tl br)
		(or (and tl br)
		    (and rows cols))))
      (error "Missing :tl/:br or :cols & :rows arguments"))
  ;; find tl & br positions
  (cl-flet* ((search (regex matchfun count) ;function to find position by regexp. Returns 'nomatch if cant be found
		     (if (re-search-forward regex nil t count)
			 (if (match-string 1)
			     (funcall matchfun 1)
			   (funcall matchfun 0))
		       (if (eq error t)
			   (error "Unable to match regex: %s" regex)
			 'nomatch)))
	     (getpos (arg matchfn)	;function to return buffer position corresponding to TL/BR ARG
		     (cond		;if ARG is nil then return nil, if position can't be found return 'nomatch
		      ((integerp arg) arg)
		      ((and (numberp arg) (>= arg 0) (<= arg 1))
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
			  (point))))
	     (adjust3 (pos cols type)	;function that returns the position of the other side of the rectangle
		      (save-excursion	;in the same row as POS, when the rectangle has COLS columns. 
			(goto-char pos)	;If TYPE is non-nil POS is assumed to be on the left hand side of the rectangle
			(move-to-column	;otherwise it is assumed to be on the right hand side.
			 (if type
			     (+ cols (current-column))
			   (- (current-column) cols)) t)
			(point)))
	     (filter (lst)		;filter a list according to indices in idxs
		     (cond ((numberp idxs) (-select-by-indices (list idxs) lst))
			   ((and idxs (listp idxs)) (-select-by-indices idxs lst))
			   (t lst)))
	     (rectcoords (tl3 br3)	;function that returns the start and end positions of all substrings that
			 (let (pairs scol (ecol (save-excursion (goto-char br3) (current-column))))
			   (goto-char tl3)
			   (setq scol (current-column)
				 width (- ecol scol))
			   (while (< (point) br3)
			     (setq pairs (cons (cons (point) (+ (point) width)) pairs))
			     (forward-line 1)
			     (move-to-column scol t))
			   (filter (reverse pairs)))))
    (let* ((tl2 (getpos tl (if inctl 'match-beginning 'match-end)))
	   (br2 (getpos br (if incbr 'match-end 'match-beginning)))
	   (msg (concat (format "(rect %s %s " tl br)
			(if inctlp (format ":inctl %s " inctl))
			(if incbrp (format ":incbr %s " incbr))
			(if rowsp (format ":rows %s " rows))
			(if colsp (format ":cols %s " cols))
			(if idxsp (format ":idxs '%s)" idxs))))
	   (strs (unless (memq 'nomatch (list tl2 br2))
		   (cond
		    ((and tl2 br2 (eq cols t))
		     (prog1 (split-string (buffer-substring-no-properties tl2 br2) "\n")
		       (if debug (extract-text-debug-next (list (cons tl2 br2)) msg))))
		    ((and tl2 br2 (eq rows t))
		     (prog1 (extract-rectangle (adjust2 tl2 t) (adjust2 br2 nil))
		       (if debug (extract-text-debug-next (rectcoords (adjust2 tl2 t) (adjust2 br2 nil)) msg))))
		    ((and tl2 br2)
		     (prog1 (extract-rectangle tl2 br2)
		       (if debug (extract-text-debug-next (rectcoords tl2 br2) msg))))
		    ((and tl2 (numberp rows) (eq cols t))
		     (prog1 (split-string (buffer-substring-no-properties
					   tl2 (adjust1 tl2 nil rows 1)) "\n")
		       (if debug (extract-text-debug-next (list (cons tl2 (adjust1 tl2 nil rows 1))) msg))))
		    ((and br2 (numberp rows) (eq cols t))
		     (prog1 (split-string (buffer-substring-no-properties (adjust1 br2 t rows 1) br2) "\n")
		       (if debug (extract-text-debug-next (list (cons (adjust1 br2 t rows 1) br2)) msg))))
		    ((and tl2 (numberp cols) (eq rows t))
		     (prog1 (extract-rectangle (adjust2 tl2 t) (adjust2 (adjust3 tl2 cols t) nil))
		       (if debug (extract-text-debug-next (rectcoords (adjust2 tl2 t) (adjust2 (adjust3 tl2 cols t) nil))
							  msg))))
		    ((and br2 (numberp cols) (eq rows t))
		     (prog1 (extract-rectangle (adjust2 (adjust3 br2 cols nil) t) (adjust2 br2 nil))
		       (if debug (extract-text-debug-next (rectcoords (adjust2 (adjust3 br2 cols nil) t) (adjust2 br2 nil))
							  msg))))
		    ((and (or tl2 br2) rows cols)
		     (prog1 (extract-rectangle (or tl2 (adjust1 br2 t rows cols))
					       (or br2 (adjust1 tl2 nil rows cols)))
		       (if debug (extract-text-debug-next (rectcoords (or tl2 (adjust1 br2 t rows cols))
								      (or br2 (adjust1 tl2 nil rows cols)))
							  msg))))))))
      (if (memq 'nomatch (list tl2 br2)) error (filter strs)))))

;;;###autoload
(cl-defun copy-rectangle-to-buffer (tl br &key (inctl t) (incbr t) rows cols)
  "Copy a rectangular region of the current buffer to a new buffer.
Return the new buffer.
The arguments are the same as for `extract-matching-rectangle' apart from 
REPS, NOERROR and JOIN which are not included. If no matching rectangle can be
found an error will be thrown."
  (let ((rect (extract-matching-rectangle
	       tl br :inctl inctl :incbr incbr :rows rows :cols cols))
	(buf (generate-new-buffer " *extracted rectangle*")))
    (with-current-buffer buf
      (insert (mapconcat 'identity rect "\n"))
      (goto-char (point-min)))
    buf))

;;;###autoload
(cl-defun extract-text-choose-prog (&optional (progs extract-text-user-progs))
  "Choose item from `extract-text-user-progs', and return arguments for `extract-text'.
Alternatively you may supply your own list of programs in the PROGS argument which should 
be a list in the same form as `extract-text-user-progs'.
If the item is a command, then its interactive form will be used to obtain arguments from
the user to apply to the list of arguments for `extract-text' which are returned."
  (let* ((name (ido-completing-read "Extraction program: "
				    (mapcar 'car progs)))
	 (all (assoc-string name extract-text-user-progs))
	 (prog (second all))
	 (postproc (third all)))
    (list (if (functionp prog)
	      (funcall (apply-interactive prog))
	    prog)
	  postproc)))

;;;###autoload
(defun extract-text-compile-prog (prg)
  "Create compiled version of `extract-text' with arguments PRG applied."
  (let* ((byte-compile-warnings '(unresolved
				  callargs redefine obsolete
				  noruntime cl-functions
				  interactive-only lexical make-local
				  mapcar constants suspicious)))
    (byte-compile `(lambda nil ,(macroexpand `(extract-text ,@prg))))))

;;;###autoload
(cl-defmacro extract-text (&rest args)
  "Extract text from current buffer according to specifications in ARGS.
ARGS should be a list/tree of wrapper functions for extracting bits of text (see below).

The text extraction is carried out in a sequential and recursive manner according 
to the elements of ARGS, and the returned results reflect the structure of ARGS. 
Each element may be either an atom or function call to be evaluated (see available
function below), a keyword arg & corresponding value (see below), or a list which 
is then traversed and evaluated recursively.
This allows for a great amount of flexibility in the extraction specification: by use
of parentheses and the :FLATTEN arg you can return arbitrary tree structures.

AVAILABLE FUNCTIONS:

As well as the usual elisp functions, the functions defined in `extract-text-builtin-wrappers',
and `extract-text-user-wrappers' (which see) are defined within the scope of the evaluation of 
ARGS, and can be used to extract text or move the cursor.

KEYWORD ARGS:

The following keyword args may be used to specify how to deal with the extractions in the current
list. 

:REPS    - number of times to repeat the current list of extractions (default 1)
:ERROR   - can take one of the following values:
             'skip : extractions that throw errors will be ignored
             'skipall : if an error is thrown then all extractions within the current repetition will be ignored
             'stop : if an error is thrown then stop iterating over repetitions, but keep extractions gathered so far
             'stopall : if an error is thrown then stop iterating over repetitions and ignore all extractions
                        within the current repetition
           if any other symbol or value is supplied then that will be used in place of the extraction which
           threw the error. If you supply a list which can be evalled then it will be evalled when an error 
           is thrown, and the return value used in place of the extraction. 

:FLATTEN - specify that returned list should be flattened to this depth with `-flatten-n' function 
:DEBUG   - if non-nil, step through extractions one by one, highlighting them and prompting the user to continue.
           This is useful when checking your extraction program.

The following keyword args are passed to `extract-matching-rectangle' (which see) to restrict the
text to a rectangle in the current buffer before performing extractions: 
  :TL, :BR, :INCTL, :INCBR, :ROWS, :COLS
This allows you to narrow down the search space for the extraction functions.

EXAMPLES:

 (extract-text (regex \"[0-9]+\\.[0-9]+\") :REPS 5 :ERROR 'NA)
 
Explanation: extract the first 5 numbers from the current buffer. If there are fewer than 5 numbers, pad with 'NA.

 A more complex example:

 (extract-text (rect \"Address:\" nil :inctl nil :rows 4 :cols 25)
               (move :bwdmark 1)
               ((regex \"Amount: *\\([0-9]+\\.[0-9]+\\)\") :REPS 3 :TL \"Address:\" :BR \"Total:\" :INCBR nil :COLS t)
               (regex \"Total: *\\([0-9]+\\.[0-9]+\\)\")
               :REPS 1000 :ERROR 'skip)

 Explanation: repeat the following extraction as many times as possible, upto 1000 times
   1) extract an address in a rectangle of 4 rows and 25 columns, 
   2) move back to a position before the address
   3) extract three \"Amount\" numbers between \"Address\" and \"Total\"
   4) extract a \"Total\" number."
  (let ((args2 args))
    ;; scope in some wrapper functions (this is why it needs to be implemented as a macro)
    `(cl-flet* (,@extract-text-builtin-wrappers
		,@extract-text-user-wrappers)
       (cl-macrolet ((recurse
		      (args3)	;do not be tempted to use &rest here, you'll get infinite recursion!
		      (if (or (not (listp args3))
			      (functionp (car args3))
			      (and (symbolp (car args3))
				   (subrp (symbol-function (car args3))))
			      (eq (car args3) 'quote)
			      (macrop (car args3))
			      (memq (car args3)
				    (remove 'nil
					    (append (mapcar 'car extract-text-builtin-wrappers)
						    (mapcar 'car extract-text-user-wrappers)))))
			  args3
			(let ((args4 args3)) ;need this let form so we can use a symbol 'args4 to access the input
			  `(let ,(extract-keyword-bindings
				  'args4 t :REPS :ERROR :TL :BR (:INCTL t) (:INCBR t) :ROWS :COLS :FLATTEN :DEBUG)
			     ;; set defaults and get buffer containing text
			     (let* ((REPS (or REPS 1))
				    (FLATTEN (or FLATTEN 0))
				    (buf2 (if (not (or TL BR)) (current-buffer)
					    ;; execute the restriction (if any) specified by the TL, BR, etc. 
					    (copy-rectangle-to-buffer
					     TL BR :inctl INCTL :incbr INCBR :rows ROWS :cols COLS)))
				    allresults positions)
			       ;; check for errors, and ignore them if ERROR is non-nil
			       (with-current-buffer buf2
				 ;; repeat the extraction for REPS repeats
				 (cl-loop named 'overreps
					  for i from 1 to REPS
					  do (progn (if (and DEBUG extract-text-reset-debug-highlights)
							(extract-text-dehighlight))
						    (let (results)
						      (condition-case err2
							  (progn
							    ,@(cl-loop 
							       for spec in args4 collect
							       `(setq positions (cons (point) positions)
								      results (remove
									       'skip
									       (cons
										(condition-case err
										    (recurse ,spec)
										  (error (if (or
											      (null ERROR)
											      (memq ERROR
												    '(skipall stop stopall)))
											     (signal (car err) (cdr err))
											   (if (and (listp ERROR)
												    (symbolp (car ERROR))
												    (or (functionp (car ERROR))
													(subrp (symbol-function
														(car ERROR)))
													(macrop (car ERROR))))
											       (eval ERROR)
											     ERROR))))
										results)))))
							(error (case ERROR
								 (skipall (setq results nil))
								 (stopall (cl-return-from 'overreps))
								 (stop (when results
									 (setq allresults
									       (cons (-flatten-1 (reverse results))
										     allresults)))
								       (cl-return-from 'overreps))
								 (t (if (or TL BR) (kill-buffer buf2))
								    (signal (car err2) (cdr err2))))))
						      (when results
							(setq allresults
							      (cons (-flatten-1 (reverse results)) allresults)))))))
			       (if (or TL BR) (kill-buffer buf2))
			       (-flatten-n FLATTEN (reverse allresults))))))))
	 (save-excursion (goto-char (point-min))
			 (prog1 (recurse ,args2)
			   (extract-text-dehighlight)))))))

;; Better to build functions from other functions rather than using a function generating macro
;; since a function generated by a macro can't be easily debugged.
;; The following pair of functions can be used at the start and end of extraction functions for
;; obtaining arguments and exporting results.
;; Any changes to one should be matched by corresponding changes in the other.

;;;###autoload
(defun extract-text-choose-export-args (&optional exclude)
  "Prompt the user for an export method for `export-text' related commands.
The return value is a list containing:
 1) A symbol or filename indicating the export method
 2) An org-table conversion function (e.g. `orgtbl-to-csv'), or nil
 3) A list of extra parameters for the conversion function, or nil
See `extract-text-from-buffers' for more details.
Optional argument EXCLUDE can be a list of options to exclude from the prompt."
  (let* ((export (let ((response (ido-completing-read
				  "Export: "
				  (set-difference '("none" "to variable" "to kill ring" "insert at point"
						    "write/append to file" "append to buffer")
						  exclude :test 'equal))))
		   (cond ((equal response "none") nil)
			 ((equal response "to variable")
			  (let ((continue t) var)
			    (while continue
			      (setq var (read-minibuffer "Variable name: " "extract-text-var-") continue nil)
			      (if (boundp var)
				  (setq continue
					(not (and (y-or-n-p (format "%s is already in use. Overwrite? " var))
						  (y-or-n-p "Are you sure? "))))))
			    var))
			 ((equal response "to kill ring") 'kill)
			 ((equal response "insert at point") 'insert)
			 ((equal response "write/append to file")
			  (cons (read-file-name "Filename: ") (y-or-n-p "Append? ")))
			 ((equal response "append to buffer")
			  (ido-read-buffer "Buffer: ")))))
	 (convfn (if (consp export)
		     (ido-completing-read "Conversion function: "
					  '("orgtbl-to-tsv" "orgtbl-to-csv" "orgtbl-to-latex"
					    "orgtbl-to-html" "orgtbl-to-generic"
					    "orgtbl-to-texinfo" "orgtbl-to-orgtbl"
					    "orgtbl-to-unicode")
					  nil t (symbol-name (org-table-get-convfn export)))))
	 (params (if (and (consp export) (y-or-n-p "Extra export parameters? "))
		     (read (concat "'(" (read-string "Parameters: ") ")")))))
    (list export (intern-soft convfn) params)))

;;;###autoload
(defun extract-text-process-results (results postproc export &optional convfn params)
  "Postprocess and then export results in RESULTS according to values of EXPORT, CONVFN & PARAMS.

POSTPROC can be a function of one argument to apply to RESULTS to get it in a more suitable
form for exporting to an org-table or file, (e.g. `-flatten-1' or `key-values-to-lists').
Otherwise if POSTPROC is nil, no extra processing will be done.

The EXPORT arg determines actions to perform with the results after processing with POSTPROC:
  nil (default) - do nothing (apart from returning results)
  'kill - save an org-table of the results to the `kill-ring' 
  'insert - insert an org-table of the results at point
  any other symbol - set value of EXPORT symbol to equal the results
  a string - save results to the file path specified in EXPORT using CONVFN to convert the results. 
             By default the file name extension will be used to choose the correct conversion 
             function (one of `orgtbl-to-tsv' `orgtbl-to-csv' `orgtbl-to-latex' `orgtbl-to-html' 
             `orgtbl-to-generic' `orgtbl-to-texinfo' `orgtbl-to-orgtbl', or `orgtbl-to-unicode'). 
             You can also use the PARAMS arg to specify parameters to pass to the PARAMS arg of the 
             conversion function (see `orgtbl-to-generic').

In all cases the function will return the results after processing with POSTPROC (if non-nil)."
  (let* ((results1 (-flatten-1 results))
	 (results2 (if postproc (funcall postproc results1) results1)))
    (cond ((eq export 'kill)
	   (kill-new (org-table-lisp-to-string results2)))
	  ((eq export 'insert)
	   (if (eq major-mode 'bs-mode) (bs-kill))
	   (save-excursion (insert (org-table-lisp-to-string results2))))
	  ((and export (symbolp export))
	   (set export results2))
	  ((consp export)
	   (let* ((filename (car export))
		  (appendp (cdr export))
		  (convfn (or convfn (org-table-get-convfn filename))))
	     (with-temp-buffer
	       (insert (funcall convfn results2 params))
	       (if (not appendp)
		   (write-file filename t)
		 (insert "\n")
		 (append-to-file (point-min) (point-max) filename)))))
	  ((stringp export)
	   (with-current-buffer export
	     (save-excursion
	       (goto-char (point-max))
	       (insert (org-table-lisp-to-string results2)))))
	  (t nil))
    results2))

;; TODO make sure result are in correct form for all export types
;;;###autoload
(defun extract-text-from-current-buffer (spec &optional postproc export convfn params debug)
  "Extract text from the current buffer, or active region.

If region is active, restrict extraction to that region.
Arguments SPEC, POSTPROC, EXPORT, CONVFN & PARAMS are the same as for `extract-text-from-buffers'.
If the DEBUG arg is a positive number or prefix arg then \":DEBUG t\" will be added to SPEC so you 
can step through the the extractions one at a time. If the DEBUG or prefix arg is negative, then 
any existing debug specification in SPEC will be removed.

Note: when target is an org-table the top level extractions appear in the same row as separate columns.
If you want multiple rows you may need to alter SPEC so that each extraction is wrapped in a list and 
then set POSTPROC to `-flatten-1'."
  (interactive (let* ((prog (extract-text-choose-prog))
		      (spec (car prog))
		      (postproc (second prog))
		      (exportargs (extract-text-choose-export-args)))
		 (list spec postproc (car exportargs) (second exportargs) (third exportargs)
		       (if current-prefix-arg (prefix-numeric-value current-prefix-arg)))))
  (when (region-active-p)
    (extract-keyword-bindings 'spec nil :TL :BR :COLS)
    (setq spec (append spec (list :TL (region-beginning) :BR (region-end)))))
  (when debug
    (extract-keyword-bindings 'spec nil :DEBUG)
    (setq spec (append spec (list :DEBUG (or (not (numberp debug)) (>= debug 0))))))
  (let ((results (funcall (extract-text-compile-prog spec))))
    (if results
	(extract-text-process-results (list results) postproc export convfn params)
      (error "No matching text found"))))

;;;###autoload
(defun extract-text-from-buffers (buffers spec &optional postproc export convfn params)
  "Extract text from buffers listed in BUFFERS or matching regexp BUFFERS.

SPEC is a quoted list containing the extraction specification for `extract-text'.
When called interactively an item of `extract-text-user-progs' will be prompted for.
BUFFERS can be either a list of buffers/buffer names, or a regexp matching the names 
of buffers to use. When called interactively in `bs-mode' (buffer-show mode) any marked 
buffers will be used, otherwise a regexp will be prompted for.
The current buffer name is scoped into the `name' variable during processing of SPEC so 
you can make use of it in the results.

The return value will be a list of lists. Each sublist will be the list returned
by `extract-text' applied to the corresponding file. If you are going to export 
this list to an org-table or to a file it might not be in the form you want.
In this case you can use the POSTPROC argument to specify a function to apply to the
list before exporting it, e.g. `key-values-to-lists' or `-flatten-1'.

The optional EXPORT, CONVFN and PARAMS arguments will be processed by `extract-text-process-results'.
See the documentation of that function for further details."
  (interactive (let* ((buffers (if (and (eq major-mode 'bs-mode) bs--marked-buffers)
				   bs--marked-buffers
				 (read-regexp "Enter regexp matching buffer names: ")))
		      (prog (extract-text-choose-prog))
		      (spec (car prog))
		      (postproc (second prog))
		      (exportargs (extract-text-choose-export-args)))
		 (list buffers spec postproc (car exportargs) (second exportargs) (third exportargs))))
  (let* ((buffers (if (stringp buffers)
		      (cl-loop for buf in (buffer-list)
			       for name = (buffer-name buf)
			       when (string-match buffers name)
			       collect name)
		    buffers))
	 (extractfn (extract-text-compile-prog spec))
	 (results (cl-loop for buf in buffers
			   for name = (if (stringp buf) buf (buffer-name buf))
			   do (message "Processing buffer: %s" name)
			   collect (with-current-buffer buf (funcall extractfn)))))
    (extract-text-process-results results postproc export convfn params)))

;;;###autoload
(defun extract-text-from-files (files spec &optional postproc export convfn params)
  "Extract text from FILES according to specification SPEC.

SPEC is a quoted list containing the extraction specification for `extract-text'.
When called interactively an item of `extract-text-user-progs' will be prompted for.
FILES can be either a list of filepaths or a wildcard pattern matching several 
filepaths (see `file-expand-wildcards'). When called interactively in `dired-mode'
then `dired-map-over-marks' will be used to select which FILES to use, otherwise a 
wildcard pattern will be prompted for. The current file name is scoped into the 
`name' variable during processing of SPEC so you can make use of it in the results.

All other arguments are the same as for `extract-text-from-buffers'."
  (interactive (let* ((files (if (eq major-mode 'dired-mode)
				 (save-excursion
				   (dired-map-over-marks (dired-get-filename) current-prefix-arg))
			       (ido-read-file-name "Enter wildcard expression matching filenames:\n")))
		      (prog (extract-text-choose-prog))
		      (spec (car prog))
		      (postproc (second prog))
		      (exportargs (extract-text-choose-export-args)))
		 (list buffers spec postproc (car exportargs) (second exportargs) (third exportargs))))
  (let* ((files (cond ((stringp files) (file-expand-wildcards files))
		      ((listp files) files)
		      (t (error "Invalid argument for files"))))
	 (extractfn (extract-text-compile-prog spec))
	 (results (cl-loop for name in files
			   for bufexists = (find-buffer-visiting name)
			   for buf = (if (file-readable-p name) (find-file-noselect name))
			   do (message "Processing file: %s" name)
			   if buf collect (prog1 (with-current-buffer buf
						   (prog1 (funcall extractfn)
						     (set-buffer-modified-p nil)))
					    (unless bufexists (kill-buffer buf))))))
    (extract-text-process-results results postproc export convfn params)))

;;;###autoload
(defun org-table-lisp-to-string (lst &optional insert)
  "Convert an org table stored in list LST into a string.
LST should be a list of lists as returned by `org-table-to-lisp'.
If optional arg INSERT is non-nil then insert and align the table at point."
  (if lst
      (let* ((ncols (-max (mapcar (lambda (x) (if (listp x) (length x) 1)) lst)))
             (str (mapconcat (lambda(x)
                               (if (eq x 'hline) (concat "|" (s-repeat ncols "-|") "\n")
                                 (concat "| " (mapconcat 'identity x " | " ) "  |\n" )))
                             lst "")))
        (if (not insert) str
          (insert str)
          (org-table-align)))))

;;;###autoload
(defun org-table-get-convfn (file)
  "Return appropriate function for converting org-tables to format matching FILE."
  (let* ((formats '("orgtbl-to-tsv" "orgtbl-to-csv" "orgtbl-to-latex"
		    "orgtbl-to-html" "orgtbl-to-generic"
		    "orgtbl-to-texinfo" "orgtbl-to-orgtbl"
		    "orgtbl-to-unicode"))
	 (fileext (and file (file-name-extension file)))
	 (deffmt-readable
	   (if fileext
	       (replace-regexp-in-string
		"\t" "\\t"
		(replace-regexp-in-string
		 "\n" "\\n"
		 (or (car (delq nil
				(mapcar
				 (lambda (f)
				   (and (org-string-match-p fileext f) f))
				 formats)))
		     org-table-export-default-format)
		 t t) t t)
	     org-table-export-default-format)))
    (intern-soft deffmt-readable)))

;;;###autoload
(cl-defun key-values-to-lists (lst &optional missing)
  "Convert key-value lists LST into a key list and values lists.
LST should be a list of lists, each of which contains lists of key-value pairs,
e.g: '(((\"val1\" 1.2) (\"val2\" 1.9) (\"val3\" 3.1))
       ((\"val2\" 2.4) (\"val1\" 2.1))
       ((\"val3\" 5.2) (\"val2\" 3.4)))
The result will be a list of lists, the first of which contains the keys, and the subsequent
lists contain corresponding values in the same order as the keys in the first list,
e.g: '((\"val1\" \"val2\" \"val3\")
       (1.2 1.9 3.1)
       (2.1 2.4 nil)
       (nil 3.4 5.2))
Missing values for any key will be filled with the MISSING arg (nil by default).
This can be used to convert lists of key-value pairs into a csv file, or org-table."
  (let ((headers (mapcar 'car (car lst))))
    (cl-loop for sublst in lst
	     do	(setq headers (cl-union headers (mapcar 'car sublst) :test 'equal)))
    (cons headers
	  (cl-loop for sublst in lst
		   collect (cl-loop for key in headers
				    collect (or (cadr (assoc-string key sublst)) missing))))))

;;;###autoload
(defun -flatten-1 (L)
  "Apply `-flatten-n' function to L with NUM argument set to 1."
  (-flatten-n 1 L))

(provide 'extract-text)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "extract-text.el" (buffer-name) (buffer-string) "update")

;;; extract-text.el ends here
