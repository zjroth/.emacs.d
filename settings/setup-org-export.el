
(use-package htmlize)
(setq org-html-inline-images t)
;; https://gongzhitaao.org/orgcss/org.css
(setq org-html-head
      (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"file://"
              (expand-file-name "org.css" settings-dir)
              "\"/>"))

(require 'ox-org)

(require 'ox-latex)
(setq org-latex-listings 'minted)
(setq org-export-with-smart-quotes t)
(add-to-list 'org-latex-packages-alist
             '("newfloat" "minted"))
(setq org-latex-pdf-process
      '("lualatex --shell-escape --interaction nonstopmode --output-directory %o %f"
        "lualatex --shell-escape --interaction nonstopmode --output-directory %o %f"
        "lualatex --shell-escape --interaction nonstopmode --output-directory %o %f"))
;;
;; ;; Display inline images
;; (add-hook 'org-mode-hook 'org-display-inline-images)

;; ;; Allow exporting to jupyter notebooks.
;; (load-file "~/.emacs.d/other-packages/ox-ipynb/ox-ipynb.el")
;; (require 'ox-ipynb)

(use-package ox-gfm)

;; ------------------------------------------------------------------------
;; Allow modification of how checkboxes are exported to latex
;; ------------------------------------------------------------------------
;;
;; The code below introduces `org-latex-checkbox-types' and minimally modifies
;; `org-latex-item' from ox-latex.el to use the values from
;; `org-latex-checkbox-types'.  That is all.

(defvar org-latex-checkbox-types
  '((on    . "$\\boxtimes$")
    (off   . "$\\square$")
    (trans . "$\\boxminus$")))

(defun org-latex-item (item contents info)
  "Transcode an ITEM element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((counter
          (let ((count (org-element-property :counter item))
                (level
                 ;; Determine level of current item to determine the
                 ;; correct LaTeX counter to use (enumi, enumii...).
                 (let ((parent item) (level 0))
                   (while (memq (org-element-type
                                 (setq parent (org-export-get-parent parent)))
                                '(plain-list item))
                     (when (and (eq (org-element-type parent) 'plain-list)
                                (eq (org-element-property :type parent)
                                    'ordered))
                       (cl-incf level)))
                   level)))
            (and count
                 (< level 5)
                 (format "\\setcounter{enum%s}{%s}\n"
                         (nth (1- level) '("i" "ii" "iii" "iv"))
                         (1- count)))))
         (checkbox (alist-get (org-element-property :checkbox item)
                              org-latex-checkbox-types))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info))))
         ;; If there are footnotes references in tag, be sure to add
         ;; their definition at the end of the item.  This workaround
         ;; is necessary since "\footnote{}" command is not supported
         ;; in tags.
         (tag-footnotes
          (or (and tag (org-latex--delayed-footnotes-definitions
                        (org-element-property :tag item) info))
              "")))
    (concat counter
            "\\item"
            (cond
             ((and checkbox tag)
              (format "[{%s %s}] %s" checkbox tag tag-footnotes))
             ((or checkbox tag)
              (format "[{%s}] %s" (or checkbox tag) tag-footnotes))
             ;; Without a tag or a check-box, if CONTENTS starts with
             ;; an opening square bracket, add "\relax" to "\item",
             ;; unless the brackets comes from an initial export
             ;; snippet (i.e. it is inserted willingly by the user).
             ((and contents
                   (string-match-p "\\`[ \t]*\\[" contents)
                   (not (let ((e (car (org-element-contents item))))
                          (and (eq (org-element-type e) 'paragraph)
                               (let ((o (car (org-element-contents e))))
                                 (and (eq (org-element-type o) 'export-snippet)
                                      (eq (org-export-snippet-backend o)
                                          'latex)))))))
              "\\relax ")
             (t " "))
            (and contents (org-trim contents)))))

(provide 'setup-org-export)
