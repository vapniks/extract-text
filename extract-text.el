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
;; Package-Requires: 
;;
;; Features that might be required by this library:
;;
;; cl
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
(defun extract-matching-strings (regexp &optional startpos endpos noerror)
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
					    rows cols startpos endpos noerror)
  "Extract a rectangle of text from the current buffer.
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

 3) By a mixture of the above two methods.

 4) By specifying just one of :START or :END (as a position or regular expression), 
    and also specifying the number of :COLS and :ROWS of the rectangle. 
    If COLS or ROWS is negative then the columns/rows are counted in the opposite 
    direction to normal, i.e. backwards from START position or forwards from END 
    position. This allows you to specify the rectangle from any corner position.

By default regexp searches for start and end positions will start from the current cursor
position up until (point-max). If STARTPOS is supplied then searches will start from that 
position instead, and if ENDPOS is supplied they will not pass that point.
If no matching rectangle is found then an error is thrown unless :NOERROR is non-nil."
  ;; check we have the required arguments
  (if (not (and (or start end)
		(or (and start end)
		    (and cols rows))))
      (error "Missing :start/:end or :cols & :rows arguments"))
  (if startpos (goto-char startpos))
  ;; find start & end positions
  (let* ((startmatch (if incstart 'match-beginning 'match-end))
	 (endmatch (if incend 'match-end 'match-beginning))
	 (start2 (cond
		    ((numberp start) start)
		    ((stringp start) (if (re-search-forward start endpos t)
					 (if (match-string 1)
					     (funcall startmatch 1)
					   (funcall startmatch 0))
				       (if noerror 'nomatch
					 (error "Unable to match start arg: %s" start))))))
	 (end2 (cond
		  ((numberp end) end)
		  ((stringp end) (if (re-search-forward end endpos t)
				     (if (match-string 1)
					 (funcall endmatch 1)
				       (funcall endmatch 0))
				   (if noerror 'nomatch
				     (error "Unable to match end arg: %s" end)))))))
    (unless (memq 'nomatch (list start2 end2))
      (setq start2 (or start2
		       (save-excursion
			 (goto-char end2)
			 (let ((col (current-column))) 
			   (forward-line (- 1 rows))
			   (move-to-column (- col (1- cols)) t)
			   (point))))
	    end2 (or end2
		     (save-excursion
		       (goto-char start2)
		       (let ((col (current-column)))
			 (forward-line (1- rows))
			 (move-to-column (+ col (1- cols)) t)
			 (point)))))
      (extract-rectangle start2 end2))))

;;;;###autoload
;; (cl-defun extract-matching-block (&key start incstart match end incend check)
;;   "Extract a block of adjacent line parts from the current buffer.
;; An example of a block would be an address in a letter header, where each
;; line of the address may be a different (unknown) length, but they are adjacent 
;; vertically.
;; The START argument should be a regexp matching the line before the first line 
;; of the block, or if INCSTART is t, the first line of the block.
;; To define the rest of the block you can either supply a MATCH argument, or
;; an END argument, both of which should be regexp's.
;; The MATCH argument should match each line of the block between START and


;;  which
;; should be a regexp matching all lines in the block after start, or an END 
;; argument which should be a regexp matching the line following the block.

;; or the last line of the block if INCEND is non-nil.

;; If any of the regexp arguments contain non-shy parenthesised subexpressions 
;; then the string matching the first non-shy subexpression will be used to determine
;; the position rather than the whole match.

;; The CHECK argument can be used as an extra check of the lines in the block:
;; if it is 'left then only left aligned matches are accepted,
;; if it is 'right then only right aligned matches are accepted,
;; if it is 'overlap (default) then only matches with overlapping columns are accepted,
;; if it is 'any then any matches are accepted, even if the columns do not overlap."
  
  
;;   )

;; plan call above functions after copying required rectangle into separate buffer
;; limits of rectangle are defined by regexp/position/percentage args top bottom left right
;; or maybe just min & max args to main function. 
;; Better to do it this way (copying rectangle to a temp buffer), since otherwise we have
;; problems when the regexp matches partly inside the required box and partly outside

;; how to specify repetition until no match??


;; extracting keyword args from list
;; (cadr (member :top '(regex foo :bottom 2 :top 4)))

;; '((regex baa :top :bottom)
;;   '((regex foo :top ? :bottom ? :left ? :right ? :repeat 4)
;;     (rect foo bar :top :bottom :left :right :repeat t)
;;     :repeat 5))



(provide 'extract-text)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "extract-text.el" (buffer-name) (buffer-string) "update")

;;; extract-text.el ends here
