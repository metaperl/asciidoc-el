;;; asciidoc.el --- asciidoc text file development support

;; Copyright (C) 2011 Terrence Brannon <metaperl@gmail.com>

;; Author: Terrence Brannon <bauhaus@metaperl.com>
;; Created: 21 Sept 2007
;; Version: 0.2
;; Keywords: text-formatting

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;;

;; Suggested add-ons:
;; doc-mode: font-locking for asciidoc buffers
;; http://xpt.sourceforge.net/tools/doc-mode/
;; - connect asciidoc.el to doc-mode as follows:
;;   (autoload 'doc-mode "doc-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
;;   (add-hook 'doc-mode-hook
;;	  '(lambda ()
;;	     (turn-on-auto-fill)
;;	     (require 'asciidoc)))

;; Author extends thanks to:
;; Steve Youngs (JackaLX on irc://irc.freenode.net/xemacs)
;; bpalmer, twb
;; jari aalto for turning me on the M-x checkdoc-current-buffer, etc

;; Version control:
;; This software is under git version control and maybe retrieved at:
;; https://github.com/metaperl/asciidoc-el


;;; Code:

;(setq debug-on-error t)


(require 'easymenu)
(require 'cl)
(require 'apropos)


(defvar *asciidoc-indent-level* 2 "Number of spaces to indent per level.")


(defvar *delimiter-length* 70
  "How many characters to use when building a delimited block string.  4 min.")

(defun asciidoc-header (title author revision)
  "Insert asciidoc header consisting of TITLE and optional AUTHOR and REVISION."
  (interactive "sHeader title: \nsHeader author (return if none): \nsHeader revision (return if none):")
  (insert
   (concat title    "\n"
	   (make-string (length title) ?=) "\n"
	   author   "\n"
	   revision "\n"
	   "\n"
	   )))

(defun asciidoc-get-started ()
  "Start the document."
  (interactive)
  (let ((date (format-time-string "%D" (current-time))))
    (asciidoc-header "Document Title"
		     "Terrence Brannon <bauhaus@metaperl.com>"
		     date)))


(defun asciidoc-emphasized (text)
  "Insert TEXT with asciidoc emphasis formatting."
  (interactive "sText to be emphasized: ")
  (insert
   (concat "_" text "_")))


(defun asciidoc-strong (text)
  "Insert TEXT with asciidoc strong formatting."
  (interactive "sText to be strong-formatted: ")
  (insert
   (concat "*" text "*")))


(defun asciidoc-monospace (text)
  "Insert TEXT with asciidoc monospace formatting."
  (interactive "sText to be monospace formatted: ")
  (insert
   (concat "`" text "`")))

(defun asciidoc-quoted (text)
  "Insert TEXT with asciidoc quoted-text formatting."
  (interactive "sText to be enclosed in quotation marks: ")
  (insert
   (concat "``" text "''")))

(defun asciidoc-unquoted (text)
  "Insert TEXT with asciidoc unquoted text formatting."
  (interactive "sText to be non-quoted: ")
  (insert
   (concat "#" text "#")))

(defun asciidoc-passthru-triple-plus (text)
  "Insert TEXT with asciidoc triple plus passthrough formatting."
  (interactive "sText to be formatted for no change: ")
  (insert
   (concat "+++" text "+++")))

(defun asciidoc-passthru-double-dollar (text)
  "Insert TEXT with asciidoc double-dollar formatting."
  (interactive "sText to be formatted for no change except escaping special characters: ")
  (insert
   (concat "$$" text "$$")))

(defun asciidoc-superscript (text)
  "Insert TEXT with asciidoc superscript formatting."
  (interactive "sText to be formatted for superscripting: ")
  (insert
   (concat "^" text "^")))

(defun asciidoc-subscript (text)
  "Insert TEXT with asciidoc subscript formatting."
  (interactive "sText to be formatted for subscripting: ")
  (insert
   (concat "~" text "~")))

(defun asciidoc-line-break ()
  "Insert asciidoc forced line break."
  (interactive)
  (insert
   (concat " +\n")))

(defun asciidoc-horizontal-rule ()
  "Insert asciidoc <hr /> tag for HTML only."
  (interactive)
  (insert
   (concat "'''\n")))

(defun asciidoc-copyright ()
  "Insert asciidoc copyright replacement."
  (interactive)
  (insert "(C) "))


(defun asciidoc-trademark ()
  "Insert asciidoc copyright replacement."
  (interactive)
  (insert "(TM) "))

(defun asciidoc-registered-trademark ()
  "Insert asciidoc registered copyright replacement."
  (interactive)
  (insert "(R) "))


(defun asciidoc-section-title (section-level title)
  "Insert asciidoc one-line title syntax consisting of SECTION-LEVEL number of asterisks and TITLE text."
  (interactive "NNumber of equals signs (2-4):  \nsSection title:  ")
  ; " " equals-signs
  (let ((equals-signs     (make-string (1+ section-level) ?=)))
    (insert
     (concat
      "\n"
      equals-signs " " title
      "\n\n"))))


(defun asciidoc-block-title (text)
  "Insert TEXT with asciidoc block title formatting."
  (interactive "sText to be formatted as block title: ")
  (insert
   (concat "." text "\n")))

(defun asciidoc-block-id-element (text)
  "Insert TEXT with asciidoc BlockId Element formatting."
  (interactive "sText to be formatted as block-id: ")
  (insert
   (concat "[[" text "]]" "\n")))

(defun asciidoc-block-reference (block-id descriptive-text)
  "Insert asciidoc reference to a block consisting of BLOCK-ID and DESCRIPTIVE-TEXT."
  (interactive "sBlockId: \nsDescriptive text: ")
  (insert
   (concat "<<" block-id "," descriptive-text ">>"
)))


(defun asciidoc-verse-paragraph (text)
  "Insert TEXT with verse paragraph formatting."
  (interactive)
  (insert (concat "[verse]" "\n")))

(defun asciidoc-literal-paragraph (text)
  "Insert TEXT with literal paragraph formatting."
  (interactive)
  (insert (concat "  " "\n")))

(defun asciidoc-admonition-paragraph (text)
  "Insert TEXT with admonition paragraph formatting."
  (interactive)
  (insert (concat "[NOTE]" "\n")))



(defun asciidoc-delimited-block (delimiter text)
  "Make a string consisting of DELIMITER and TEXT."
  (let ((str (make-string *delimiter-length* delimiter)))
    (insert (concat str "\n" text "\n" str "\n\n"))))

(defun asciidoc-comment-block (text)
  "Create an asciidoc CommentBlock consisting of TEXT."
  (interactive "sText for comment block? ")
  (asciidoc-delimited-block ?/ text))

(defun asciidoc-passthru-block (text)
  "Create an asciidoc PassthroughBlock consisting of TEXT."
  (interactive "sText for passthru block? ")
  (asciidoc-delimited-block ?+ text))

(defun asciidoc-listing-block (text)
  "Create an asciidoc ListingBlock consisting of TEXT."
  (interactive "sText for listing block? ")
  (asciidoc-delimited-block ?- text))

(defun asciidoc-literal-block (text)
  "Create an asciidoc LiteralBlock consisting of TEXT."
  (interactive "sText for literal block? ")
  (asciidoc-delimited-block ?. text))

(defun asciidoc-sidebar-block (text)
  "Create an asciidoc SidebarBlock consisting of TEXT."
  (interactive "sText for sidebar block? ")
  (asciidoc-delimited-block ?* text))

(defun asciidoc-example-block (text)
  "Create an asciidoc ExampleBlock, using TEXT and optionally modifying the default EXAMPLE-LABEL and EXAMPLE-DESCRIPTION."
  (interactive "sText for example block? ")
  (let ((example-label (read-string "Example label? (it needs a space at the end) " "Example: "))
	(example-description (read-string "Example description? " "An example")))
    (if (not (string= "" example-label))
	(insert "[caption=" "\"" example-label "\"" "]" "\n"))
    (if (not (string= "" example-description))
	(insert "." example-description "\n"))
    (asciidoc-delimited-block ?= text)))

(defun asciidoc-quotation-block (text author source)
  "Given TEXT, AUTHOR, and SOURCE, create an asciidoc QuoteBlock."
  (interactive "sText of quotation? \nsAuthor of quotation? \nsWhere did this quotation come from? ")
  (insert
   (concat "["
	   "attribution=" "\"" author "\""
	   ","
	   "citetitle="   "\"" source "\""
	   "]" "\n"))
  (asciidoc-delimited-block ?_ text))



(defun asciidoc-compile ()
  "Create an asciidoc document."
  (interactive)
  (setq compile-command
	(concat
	 "asciidoc -a numbered -a toc -a toclevels=4" " "
	 (file-name-nondirectory (buffer-file-name))
	 ))
  (call-interactively 'compile))

(defvar *asciidoc-bullet* '("-" "*")
  "Strings representing each of the two bullet levels offered by Asciidoc.")


(defun asciidoc-bullet-item (bullet-level text)
  "At BULLET-LEVEL (1 or 2), insert TEXT."
  (interactive "NBullet level (1 or 2):  \nsText for bullet:  ")
  (let* ((level  (if (= bullet-level 2) 1 0))
	 (bullet (nth level *asciidoc-bullet*))
	 (tab-space (make-string (* level 4) ?\s)))
    (insert
     (concat
      tab-space bullet " " text "\n"))))

(defun asciidoc-numbered-list-item (item-level text)
  "At ITEM-LEVEL, insert TEXT."
  (interactive "NItem level (1 or 2):  \nsText for bullet:  ")
  (let* ((level  (if (= item-level 2) 2 1))
	 (bullet (make-string level ?.))
	 (tab-space (make-string (* (- level 1) 4) ?\s)))
    (insert
     (concat
      tab-space bullet " " text "\n"))))

(defun asciidoc-labelled-list-item (text)
  "Insert TEXT."
  (interactive "sLabel for list item: ")
  (insert
   (concat
    text "::" "\n    ")))

(defun asciidoc-bibliography-item (ref-label ref-text)
  "Insert bibliography item consisting of REF-LABEL and REF-TEXT."
  (interactive "sLabel for bib item: \nsText of bibitem: ")
  (insert
   (concat
    "+" " "  "[[[" ref-label "]]]" " " ref-text "\n")))

(defun asciidoc-href (url link-text)
  "Insert hyperlink consisting of URL and LINK-TEXT."
  (interactive "sURL: \nsText describing URL: ")
  (insert
   (concat
    url "[" link-text "]" "\n"
    )))

(defun asciidoc-relative-href (url link-text)
  "Insert hyperlink consisting of URL and LINK-TEXT."
  (interactive "sRelative path to file (anchors allowed): \nsText describing link: ")
  (insert
   (concat
    "link:"
    url "[" link-text "]" "\n"
    )))

(defun asciidoc-image-href (url link-text)
  "Insert hyperlink consisting of URL and LINK-TEXT."
  (interactive "sURL to image file: \nsText describing image (only displayed when image unavailable): ")
  (insert
   (concat
    "image:"
    url "[" "\"" link-text "\"" "]" "\n"
    )))


(let ((s "admonition-paragraph"))
  (split-string s "asciidoc"))

(setq asciidoc-global-menuspec
      (list "Doc"
	    (list
	     "Links and refs"
	      (vector "Href" 'asciidoc-href)
	      (vector "Image" 'asciidoc-image-href)
	      (vector "Relative url" 'asciidoc-relative-href)
	      (vector "Internal document references" 'asciidoc-block-reference)
	      )
	    (list
	     "Reference items"
	      (vector "Bibliography listing" 'asciidoc-bibliography-item)
	      (vector "BlockId element" 'asciidoc-block-id-element))
	    (list
	     "Bullets and lists"
	      (vector "Bulleted list" 'asciidoc-bullet-item)
	      (vector "Numbered list" 'asciidoc-numbered-list-item)
	      (vector "Labelled list" 'asciidoc-labelled-list-item)
	      )
	    (list
	     "Text formatting"
	      (vector "Emphasis" 'asciidoc-emphasized)
	      (vector "Strong (bold)" 'asciidoc-strong)
	      (vector "Monospaced" 'asciidoc-monospace)
	      (vector "Quotation marks around text" 'asciidoc-quoted)
	      (vector "Superscript" 'asciidoc-superscript)
	      (vector "Subscript" 'asciidoc-subscript)
	      )
	    (list
	     "Special symbols"
	      (vector "Copyright" 'asciidoc-copyright)
	      (vector "Trademark" 'asciidoc-trademark)
	      (vector "Registered trademark" 'asciidoc-registered-trademark)
	      )
	    (list
	     "Blocks"
	      (vector "Example block" 'asciidoc-example-block)
	      (vector "Listing block" 'asciidoc-listing-block)
	      (vector "Quotation block" 'asciidoc-quotation-block)
	      (vector "Literal block" 'asciidoc-literal-block)
	      (vector "Sidebar block" 'asciidoc-sidebar-block)

	      (vector "Comment block" 'asciidoc-comment-block)
	      (vector "Pass-through block" 'asciidoc-passthru-triple-plus)
	      )
	    (list
	     "Run Asciidoc"
	      (vector "Compile"   'asciidoc-compile)
	      (vector "Recompile" 'recompile)
	      )
	    (vector "Start document" 'asciidoc-get-started)
))


(easy-menu-define
  asciidoc-global-menu global-map "" asciidoc-global-menuspec)

(provide 'asciidoc)

;;; asciidoc.el ends here
