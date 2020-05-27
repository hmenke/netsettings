;; -*- lexical-binding: t; -*-

(require 'url)
(require 'dom)

;;;###autoload
(defun bibref-from-doi (doi)
  "Load a BibTeX record by DOI from https://doi.org/"
  (interactive "sDOI: ")
  (let* ((url (concat "https://doi.org/" doi))
         (url-mime-accept-string "application/x-bibtex"))
    (insert
     (with-current-buffer (url-retrieve-synchronously url)
       (let* ((start url-http-end-of-headers)
              (end (point-max))
              (all (buffer-string))
              (body (buffer-substring start end)))
         (replace-regexp-in-string "^\t" "  " (url-unhex-string body)))))))

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
              (updated (dom-text (car (dom-by-tag dom 'updated))))
              (year (format-time-string "%Y" (date-to-time updated)))
              (primaryClass (dom-attr (car (dom-by-tag entry 'category)) 'term))
              (key (concat (car (last (split-string (dom-text (car names))))) year))
              (url (concat "https://arxiv.org/abs/" eprint)))
         (format bibref/arxiv-format-string key title author year eprint primaryClass url))))))

(provide 'bibref)
