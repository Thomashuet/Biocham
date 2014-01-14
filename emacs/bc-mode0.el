; BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
; Copyright 2003-2006, INRIA, Projet Contraintes
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
; bc-mode.el

; TODO indent

;;;;;(defun bc-indent-line ()
;;;;;   "Indent current line as Biocham code"
;;;;;   (interactive)
;;;;;   (beginning-of-line)
;;;;;   (if (bobp)  ; At the start of the file use zero indent.
;;;;;      (indent-line-to 0)
;;;;;   (let ((not-indented t) cur-indent)
;;;;;   (if (looking-at "^[ \t]*for") ; if line starts with 'for' 1 tab
;;;;;      (setq cur-indent default-tab-width)
;;;;;   (if (looking-at "^[ \t]*and") ; if line starts with 'and'
;;;;;      (save-excursion
;;;;;         (forward-line -1)
;;;;;         (beginning-of-line)
;;;;;         (if (looking-at "^[ \t]*where") ; and previous is 'where' -> same
;;;;;            (setq cur-indent (current-indentation))
;;;;;            ; else add 1 tab width
;;;;;            (setq cur-indent (+ (current-indentation) default-tab-width))))
;;;;;   (if (looking-at "^[ \t]*else") ; if line starts with 'else'
;;;;;      (save-excursion
;;;;;         (forward-line -1)
;;;;;         (beginning-of-line)
;;;;;         (if (looking-at "^[ \t]*then") ; and previous is 'then' -> same
;;;;;            (setq cur-indent (current-indentation))
;;;;;            ; else add 1 tab width
;;;;;            (setq cur-indent (+ (current-indentation) default-tab-width))))
;;;;;   (save-excursion
;;;;;      (forward-line -1)
;;;;;      (beginning-of-line)
;;;;;      (if (looking-at "^[ \t]*%") ; previous was a comment -> same
;;;;;         (setq cur-indent (current-indentation))
;;;;;      (if (looking-at "^.*.[ \t]$") ; end of rule/def -> 0
;;;;;         (setq cur-indent 0)
;;;;;         ; else add 1 tab width
;;;;;         (setq cur-indent (+ (current-indentation) default-tab-width)))))))))
;;;;;   (indent-line-to cur-indent)))

; (require 'generic-x)

(defvar bc-mode-hook nil)

(defvar biocham-mode-keywords '(
