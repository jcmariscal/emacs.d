;; custom-rebox2.el --- set's up rebox defaults
;; Description:

;; Copyright (C) 2021 JC Mariscal <jc0x0b@gmai.com>
;; --------------------------------------------------------------------------
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;; --------------------------------------------------------------------------


;; usage: M-126 M-Shift-q   - set style #126
;;        M-Shift-q         - shuffle box styles

;; ================================== ;;
;; explanation of style convention:   ;;
;; ================================== ;;

;; languages
;; ---------
;; 100 unknown
;; 200 /* */
;; 300 //
;; 400 #
;; 500 ;
;; 600 #

;; box quality
;; -----------
;; 10 simple
;; 20 rounded
;; 40 starred
;; 221 /* */
;; 11 deletes box

;; ------------
;; other langs
;; ------------
;; thickness of characters
;; 10 1char wide
;; 20 2char wide
;; 30 3char wide
;; 40 4 char wide
;; C++ use quality 20 as base

;; style is registered as x25 where x is the language
;; eg: 525   - 5 is ; comment style and 25 is the style number
;; eg: 246   - 2 is C style comment and 46 is the style number

(require 'rebox2)

(setq rebox-style-loop '(21 75 76 77 78 46 47 48 49 50 11)) ;
(setq custom-rebox2-lisp-mode-box-style-list '(21 75 76 77 78 11))

(global-set-key [(meta shift q)] 'rebox-cycle)
;;(global-set-key [(meta q)] 'rebox-dwim)

(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (set (make-local-variable 'rebox-style-loop) custom-rebox2-lisp-mode-box-style-list)
				  (set (make-local-variable 'rebox-min-fill-column) 40)
				  (rebox-mode 1)))

(provide 'custom-rebox2)


;,---------------------------------------------------------------------------------------------------------------------------------------------------
;| how to add new template: https://stackoverflow.com/questions/18211534/emacs-make-square-block-comments-with-right-aligned-border/18213214#18213214
;`---------------------------------------------------------------------------------------------------------------------------------------------------

;; examples:
; The following template defines the specific style required here,
; which does not correspond to any built-in rebox2 style.
;
; "75" means that the style is registered as x75, where "x" depends
; on the current langauge mode. The "?" char is switched for the language
; specific comment char
;
; "999" is the weighting used for recognising this comment style.
; This value works for me.
;(rebox-register-template
; 75
; 999
; '("?*************?"
;   "?* box123456 *?"
;   "?*************?"))
;
;(add-hook 'perl-mode-hook (lambda ()
; The "style loop" specifies a list of box styles which rebox will cycle
; through if you refill (M-q) a box repeatedly. Having "11" in this loop
; will allow you to easily "unbox" a comment block, e.g. for "uncomment-region"
;                (set (make-local-variable 'rebox-style-loop) '(75 11))
; The "min-fill-column" setting ensures that the box is not made narrower
; when the text is short
;                (set (make-local-variable 'rebox-min-fill-column) 79)
;                (rebox-mode 1)))
