;; -*- lexical-binding: t; -*-

(require 'url)
(require 'dom)

;;;###autoload
(defun bibref-from-aps (doi)
  "Download a BibTeX record from the APS journals"
  (interactive "sAPS DOI: ")
  (let ((url (concat "https://journals.aps.org/prl/export/" doi "?type=bibtex&download=true")))
    (insert
     (with-current-buffer (url-retrieve-synchronously url)
       (let* ((start url-http-end-of-headers)
              (end (point-max))
              (all (buffer-string))
              (body (buffer-substring start end)))
         (url-unhex-string body))))))

;;;###autoload
(defun bibref-from-doi (doi)
  "Load a BibTeX record by DOI from https://doi.org/"
  (interactive "sDOI: ")
  (let* ((url (concat "https://doi.org/" doi))
         (url-mime-accept-string "application/x-bibtex")
         (pnt (point)))
    (insert
     (with-current-buffer (url-retrieve-synchronously url)
       (let* ((start url-http-end-of-headers)
              (end (point-max))
              (all (buffer-string))
              (body (buffer-substring start end)))
         (decode-coding-string (url-unhex-string body)'utf-8))))
    (set-mark pnt)
    (activate-mark)
    (call-interactively 'bibtex-reformat)
    (call-interactively 'bibtex-end-of-entry)))

(defvar bibref/arxiv-format-string "@electronic{%s,
  title = {%s},
  author = {%s},
  archivePrefix = {arXiv},
  year = {%s},
  eprint = {%s},
  primaryClass = {%s},
  url = {%s},
}")

;;;###autoload
(defun bibref-from-arxiv (eprint)
  "Parse the arXiv website and build a BibTeX record"
  (interactive "sarXiv: ")
  (let ((url (concat "http://export.arxiv.org/api/query?id_list=" eprint)))
    (insert
     (with-current-buffer (url-retrieve-synchronously url)
       (let* ((start (progn (goto-char url-http-end-of-headers)
                            (search-forward "<?xml ")
                            (match-beginning 0)))
              (end (point-max))
              (dom (libxml-parse-xml-region start end))
              (entry (car (dom-by-tag dom 'entry)))
              (title (dom-text (car (dom-by-tag entry 'title))))
              (names (dom-by-tag entry 'name))
              (author (mapconcat 'dom-text names " and "))
              (published (dom-text (car (dom-by-tag entry 'published))))
              (year (format-time-string "%Y" (date-to-time published)))
              (primaryClass (dom-attr (car (dom-by-tag entry 'category)) 'term))
              (key (concat (car (last (split-string (dom-text (car names))))) year))
              (url (concat "https://arxiv.org/abs/" eprint)))
         (format bibref/arxiv-format-string key title author year eprint primaryClass url))))))

(provide 'bibref)
