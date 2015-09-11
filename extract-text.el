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
;; Package-Requires: ((dash "20150829.433"))
;;
;; Features that might be required by this library:
;;
;; cl, dash
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
;; This library provides functions for
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
(cl-defun extract-matching-rectangle (start end &key (incstart t) (incend t)
					    rows cols startpos endpos noerror join)
  "Extract a rectangle of text from the current buffer.
The return value is a list of strings (the lines of the rectangle), or if :JOIN is non-nil
a single string formed by concatenating the rectangle lines with JOIN as a separator.

The rectangle can be specified in several different ways:

 1) By passing corner positions to :START and :END (see `extract-rectangle')

 2) By passing regular expressions to :START and :END which match strings delimiting the
    corner positions. If the regexp contains any non-shy subexpressions, the first one will 
    be used for determining the start/end match.
    By default the matched strings will be included in the rectangle. To exclude the matched 
    start/end text so the rectangle starts/ends after/before the matched text you must set 
    :INCSTART and/or :INCEND to nil (they are t by default).
    The search for matching strings will start from the beginning of the buffer/string 
    unless the :STARTPOS argument is supplied giving the start position.

 3) By passing cons cells in the form (rep . regex) to :START and :END. In this case the
    rep'th match to regex will be used for the start/end position.

 4) By a mixture of the above methods.

 5) By specifying just one of :START or :END (as a position, regexp, or cons cell), 
    and also specifying the number of :COLS and :ROWS of the rectangle. 
    If COLS or ROWS is negative then the columns/rows are counted in the opposite 
    direction to normal, i.e. backwards from START position or forwards from END 
    position. This allows you to specify the rectangle from any corner position.

By default regexp searches for start and end positions will start from the current cursor
position up until (point-max). If :STARTPOS is supplied then searches will start from that 
position instead, and if :ENDPOS is supplied they will not pass that point.
If no matching rectangle is found then an error is thrown unless :NOERROR is non-nil."
  ;; check we have the required arguments
  (if (not (and (or start end)
		(or (and start end)
		    (and cols rows))))
      (error "Missing :start/:end or :cols & :rows arguments"))
  (if startpos (goto-char startpos))
  ;; find start & end positions
  (cl-flet ((search (regex matchfun count)
		    (if (re-search-forward regex endpos t count)
			(if (match-string 1)
			    (funcall matchfun 1)
			  (funcall matchfun 0))
		      (if noerror 'nomatch
			(error "Unable to match regex: %s" regex))))
	    (adjust (begin type)
		    (save-excursion
		      (goto-char begin)
		      (let ((col (current-column))) 
			(forward-line (if type (- 1 rows) (- rows 1)))
			(move-to-column (if type (- col (1- cols))
					  (+ col (1- cols))) t)
			(point)))))
    (let* ((smatchfun (if incstart 'match-beginning 'match-end))
	   (ematchfun (if incend 'match-end 'match-beginning))
	   (start2 (cond
		    ((numberp start) start)
		    ((stringp start) (search start smatchfun nil))
		    ((and (listp start)
			  (stringp (car start))
			  (integerp (cdr start)))
		     (search (car start) smatchfun (cdr start)))))
	   (end2 (cond
		  ((numberp end) end)
		  ((stringp end) (search end ematchfun nil))
		  ((and (listp end)
			(stringp (car end))
			(integerp (cdr end)))
		   (search (car end) ematchfun (cdr end))))))
      (unless (memq 'nomatch (list start2 end2))
	(setq start2 (or start2 (adjust end2 t))
	      end2 (or end2 (adjust start2 nil)))
	(if join (mapconcat 'identity (extract-rectangle start2 end2) join)
	    (extract-rectangle start2 end2))))))


(cl-defun copy-rectangle-to-buffer (start end &key (incstart t) (incend t)
					  rows cols startpos endpos)
  "Copy a rectangular region of the current buffer to a new buffer.
Return the new buffer.
The arguments are the same as for `extract-matching-rectangle' apart from 
NOERROR and JOIN which are not included."
  (let ((buf (generate-new-buffer " *extracted rectangle*"))
	(rect (extract-matching-rectangle
	       start end
	       :incstart incstart :incend incend :rows rows
	       :cols cols :startpos startpos :endpos endpos)))
    (with-current-buffer buf
      (insert (mapconcat 'identity rect "\n"))
      (goto-char (point-min)))
    buf))

(defun extract-keyword-arg (key lstsym &optional pred)
  "Remove KEY & following item from list referenced by LSTSYM, and return item.
LSTSYM should be a symbol whose value is a list.
If KEY is not in the list then return nil.
If predicate function PRED is supplied then an error will be thrown if 
PRED returns nil when supplied with the key value as argument."
  (let* ((lst (eval lstsym))
	 (ind (-elem-index key lst)))
    (unless (not ind)
      (set lstsym (append (-take ind lst) (-drop (+ 2 ind) lst)))
      (let ((val (nth ind lst)))
	(if (and pred (not (funcall pred val)))
	    (error "Invalid value for %S" key)
	  val)))))


;; plan call above functions after copying required rectangle into separate buffer
;; limits of rectangle are defined by regexp/position/percentage args top bottom left right
;; or maybe just min & max args to main function. 
;; Better to do it this way (copying rectangle to a temp buffer), since otherwise we have
;; problems when the regexp matches partly inside the required box and partly outside

;; how to specify repetition until no match??


;; (cl-defun extract-text (expr)
;;   "Extract text from buffer."
;;   (let ((funcs '(regex rect)))
;;     (cl-loop for elem in expr
;; 	     if (memq (car elem) funcs)
;; 	     (eval elem)
;; 	     else
;; 	     (let ((rep (cadr (member :rep elem)))
		   
;; 		   )

;; 	       )
;; 	     ))
;;   )



(provide 'extract-text)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "extract-text.el" (buffer-name) (buffer-string) "update")

;;; extract-text.el ends here
