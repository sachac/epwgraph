;;; epwgraph.el --- Trying to make sense of Pipewire -*- lexical-binding: t; -*-

;; Version: 0.0.1
;; Maintainer: Sacha Chua <sacha@sachachua.com>
;; Author: Random User
;; Keywords: convenience, files, hypermedia, multimedia
;; URL: https://github.com/sachac/subed
;; Package-Requires: ((emacs "25.1"))

;;; License:
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; epwgraph-show tries to visualize and manage a subset of PipeWire audio connections

;;; Code:

(require 'seq)

(defvar epwgraph-focus-history nil)
(defvar epwgraph-exclude-history nil)
(defvar epwgraph-focus-regexp nil)
(defvar epwgraph-exclude-regexp nil)
;; Haven't really worked on the case where this is nil yet
(defvar epwgraph-combine-channels t "*Non-nil means simplify the graph by combining multiple channels of the same device.")
(defvar epwgraph-display-registry '(epwgraph-show-logical-names epwgraph-show-ports))
(defvar epwgraph-display-function 'epwgraph-show-logical-names)



(defvar-keymap epwgraph-text-mode-map
  "g" #'epwgraph-refresh
  "d" #'epwgraph-disconnect-logical-nodes
  "D" #'epwgraph-disconnect-ports
  "c" #'epwgraph-connect-logical-nodes
  "C" #'epwgraph-connect-ports
  "s" #'epwgraph-show
  "w" #'write-file
  "t" #'epwgraph-cycle-display-function
  "q" #'kill-current-buffer)

(defvar-keymap epwgraph-mode-map
  :parent epwgraph-text-mode-map)

(define-derived-mode epwgraph-mode image-mode "PipeWire graph"
  "Major mode for visualizing PipeWire connections.")

(define-derived-mode epwgraph-text-mode special-mode "PipeWire graph"
  "Major mode for visualizing PipeWire connections.")

(defun epwgraph-show (&optional focus-regexp exclude-regexp)
  "Render the graph."
  (interactive
   (if current-prefix-arg
       (list (read-string "Focus: " nil 'epwgraph-focus-history)
             (read-string "Exclude: " nil 'epwgraph-exclude-history))
     (list
      epwgraph-focus-regexp
      epwgraph-exclude-regexp)))
  (let* ((buf-name "*epwgraph*"))
    (setq epwgraph-focus-regexp focus-regexp)
    (setq epwgraph-exclude-regexp exclude-regexp)
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t)
            (inhibit-redisplay t))
        (with-silent-modifications
          (erase-buffer)
          (if (display-graphic-p)
              (epwgraph-show-svg focus-regexp exclude-regexp)
            (epwgraph-show-text focus-regexp exclude-regexp))))
      (switch-to-buffer (current-buffer)))))

(defun epwgraph-show-svg (&optional focus-regexp exclude-regexp)
  (let* ((dot-content (epwgraph--filter-graph focus-regexp exclude-regexp))
         (img-file (make-temp-file "epwgraph-" nil ".svg")))
    (with-temp-buffer
      (insert dot-content)
      (call-process-region (point-min) (point-max) "dot" nil t nil "-Tsvg" "-o" img-file))
    (erase-buffer)
    (insert-file-contents img-file)
    (delete-file img-file)
    (if (not (derived-mode-p 'epwgraph-mode))
        (epwgraph-mode)
      (image-toggle-display-image))))

(defun epwgraph-show-text (&optional focus-regexp exclude-regexp)
  (let* ((dot-content (epwgraph--filter-graph focus-regexp exclude-regexp))
         (buffer (current-buffer)))
    (erase-buffer)
    (insert dot-content)
    (call-process-region (point-min) (point-max) "graph-easy" t t nil "--ascii")
    (unless (derived-mode-p 'epwgraph-text-mode) (epwgraph-text-mode))))

(defun epwgraph-show-logical-names (o)
  (epwgraph--get-logical-name (cdr o)))

(defun epwgraph-show-logical-ports (o)
  (cdr o))

(defun epwgraph-cycle-display-function ()
  "Move to the next `epwgraph-display-function' in `epwgraph-display-registry'"
  (interactive)
  (let ((pos (seq-position epwgraph-display-function epwgraph-display-registry)))
    (setq epwgraph-display-function
          (elt epwgraph-display-registry
               (if pos
                   (elt (mod (1+ pos) (length epwgraph-display-registry)))
                 0))))
  (epwgraph-refresh))

(defun epwgraph--get-id (string)
  (if (string-match "^[ \t]*\\([0-9]+\\)\\([ \t]+.*\\)?$" string)
      (string-to-number (match-string 1 string))
    string))

(defun epwgraph--get-name (string)
  (if (string-match "^[ \t]*\\([0-9]+\\)[ \t]+\\(.*\\)$" string)
      (match-string 2 string)
    string))

(defun epwgraph--get-logical-name (full-name)
  "Convert 'Device:port_FL' to 'Device:port'.
Removes common suffixes like _FL, _FR, _L, _R, .left, .right."
  (if (and epwgraph-combine-channels (string-match "\\(.*?\\)_[A-Z]+$" full-name))
      (match-string 1 full-name)
    full-name))

(defun epwgraph--get-all-links ()
  "Get all active links.
Entries are of the form (id src-id dest-id)."
  (let ((raw (shell-command-to-string "pw-link -Il"))
        source)
    (thread-last (split-string (string-trim raw) "\n" t)
                 (seq-keep
                  (lambda (line)
                    (let ((parts (split-string line " |-> ")))
                      (pcase (length parts)
                        (2 (list
                            (string-to-number (car parts))
                            source
                            (epwgraph--get-id (string-trim (cadr parts)))))
                        (1 (setq source (epwgraph--get-id (car parts)))
                           nil)))))
                 (seq-filter #'identity))))

(defun epwgraph--get-all-nodes ()
  "Get all available nodes with IDs stripped."
  (let ((raw (json-parse-string
              (shell-command-to-string "pw-dump")
              :object-type 'alist
              :array-type 'list)))
    (seq-filter
     ;; For some reason, these seem to be the nodes I'm looking for
     (lambda (o) (string-match ":" (cdr o)))
     (seq-keep
      (lambda (o)
        (let-alist o
          (when (or (alist-get 'port.name .info.props)
                    (alist-get 'port.alias .info.props)
                    (alist-get 'media.name .info.props))
            (cons .id
                  (or
                   (and (alist-get 'media.name .info.props)
                        (concat (if (alist-get 'application.name .info.props)
                                    (concat (alist-get 'application.name .info.props) ": ")
                                  "")
                                (alist-get 'media.name .info.props)))
                   (alist-get 'port.alias .info.props)
                   (alist-get 'port.name .info.props))))))
      raw))))

(defun epwgraph--simplify-channels (nodes)
  (mapcar
   (if epwgraph-combine-channels
       (lambda (o)
         (cons (car o)
               (epwgraph--get-logical-name (cdr o))))
     #'identity)
   (epwgraph--get-all-nodes)))

(defun epwgraph--automatic-connections (nodes)
  "Return a list of (id . id) edges between playbacks and monitors, loopback inputs and outputs."
  (let ((edges '())
        (nodes-list (if (hash-table-p nodes)
                        (hash-table-keys nodes)
                      nodes)))
    (dolist (src nodes-list)
      (let ((src-name (cdr src)))
        (cond
         ((string-match "\\(.*\\):playback" src-name)
          (let ((base (match-string 1 src-name)))
            (dolist (dst nodes-list)
              (when (string-match (concat
                                   (regexp-quote base) ":monitor"
                                   (if (or epwgraph-combine-channels (null (epwgraph--get-channel (cdr src))))
                                       ""
                                     (concat
                                      "_"
                                      (regexp-quote (epwgraph--get-channel (cdr src))))))
                                  (cdr dst))
                (push (list nil (car src) (car dst)) edges)))))
         ((string-match "\\(.*\\):input" src-name)
          (let ((base (match-string 1 src-name)))
            (dolist (dst nodes-list)
              (when (string-match (concat
                                   (regexp-quote base)
                                   ":\\(output\\|monitor\\)"
                                   (if (or epwgraph-combine-channels (null (epwgraph--get-channel (cdr src))))
                                       ""
                                     (concat
                                      "_"
                                      (regexp-quote (epwgraph--get-channel (cdr src))))))
                                  (cdr dst))
                (push (list nil (car src) (car dst)) edges))))))))
    (delete-dups edges)))

(defun epwgraph--filter-graph (&optional focus-regexp exclude-regexp)
  "Build a DOT string using normalized descriptive names."
  (let* ((links (epwgraph--get-all-links))
         (all-nodes
          (epwgraph--simplify-channels (epwgraph--get-all-nodes)))
         (remaining-nodes
          (if (and exclude-regexp (not (string-empty-p exclude-regexp)))
              (seq-remove (lambda (o) (string-match exclude-regexp (cdr o)))
                          all-nodes)
            all-nodes))
         (remaining-links (seq-filter (lambda (o)
                                        (and (alist-get (elt o 1) remaining-nodes)
                                             (alist-get (elt o 2) remaining-nodes)))
                                      links))
         (final-nodes (if (and focus-regexp (not (string-empty-p focus-regexp)))
                          (epwgraph--get-related-nodes
                           (seq-filter (lambda (o) (string-match focus-regexp (cdr o))) remaining-nodes)
                           remaining-links
                           remaining-nodes)
                        remaining-nodes)))
    (concat "digraph pipewire {\n"
            "  node [shape=rect, style=filled, fillcolor=\"#fdf6e3\", fontname=\"Sans\", fontsize=10];\n"
            "  edge [color=\"#586e75\"];\n"
            "  rankdir=LR;\n"
            "  bgcolor=\"transparent\";\n"
            (mapconcat (lambda (n) (format "  \"%s\";" n)) (seq-uniq (mapcar 'cdr final-nodes)) "\n")
            "\n"
            (string-join
             (seq-uniq
              ;; TODO: Color automatic connections differently
              (mapcar (lambda (o) (format "  \"%s\" -> \"%s\";"
                                          (alist-get (elt o 1) final-nodes)
                                          (alist-get (elt o 2) final-nodes)))
                      (append
                       (seq-filter (lambda (o) (and (alist-get (elt o 1) final-nodes)
                                                    (alist-get (elt o 2) final-nodes)))
                                   remaining-links)
                       (epwgraph--automatic-connections final-nodes))))
             "\n")
            "\n}")))

(defun epwgraph--get-related-nodes (start-nodes edges all-nodes)
  "Breadth-first search for finding connected components.
Return a list of nodes."
  (let ((visited (make-hash-table :test 'equal))
        (stack start-nodes)
        result)
    (while stack
      (let ((node (pop stack)))
        (unless (gethash (car node) visited)
          (puthash (car node) t visited)
          (push node result)
          (dolist (edge edges)
            (dolist (node-id (list (elt edge 1) (elt edge 2)))
              (when (and (not (gethash node-id visited))
                         (assoc node-id all-nodes))
                (push (assoc node-id all-nodes) stack)))))))
    result))

(defun epwgraph-refresh ()
  (interactive)
  (epwgraph-show epwgraph-focus-regexp epwgraph-exclude-regexp))

(defun epwgraph-complete-port (&optional prompt)
  "Use PROMPT to ask for a port name.
Returns (id . name)."
  (let* ((collection
          (seq-keep (lambda (o)
                      (cons (cdr o) o))
                    (epwgraph--get-all-nodes)))
         (value (completing-read (or prompt "Port: ")
                                 collection)))
    (assoc-default value collection #'string=)))

(defun epwgraph-complete-logical-node-name (&optional prompt)
  "Use PROMPT to ask for a logical node name.
Returns a list of ((id . name) (id . name) ...)."
  (let* ((all-nodes (epwgraph--get-all-nodes))
         (collection
          (seq-group-by
           (lambda (o)
             (epwgraph--get-logical-name (cdr o)))
           all-nodes))
         (value (completing-read (or prompt "Node: ") collection)))
    (assoc-default value collection #'string=)))

(defun epwgraph-ports-for-logical-name (s)
  (seq-filter
   (lambda (o)
     (string= s (epwgraph--get-logical-name (cdr o))))
   (epwgraph--get-all-nodes)))

(defun epwgraph-complete-existing-link (&optional prompt)
  (let* ((all-nodes (epwgraph--get-all-nodes))
         (all-links (epwgraph--get-all-links))
         (collection
          (mapcar
           (lambda (o)
             (cons
              (format "%s -> %s"
                      (alist-get (elt o 1) all-nodes)
                      (alist-get (elt o 2) all-nodes))
              o))
           all-links))
         (value (completing-read (or prompt "Link: ") collection)))
    (assoc-default value collection #'string=)))

(defun epwgraph-complete-existing-logical-link (&optional prompt)
  (let* ((all-nodes (epwgraph--get-all-nodes))
         (all-links (epwgraph--get-all-links))
         (collection
          (seq-group-by
           (lambda (o)
             (format "%s -> %s"
                     (epwgraph--get-logical-name (alist-get (elt o 1) all-nodes))
                     (epwgraph--get-logical-name (alist-get (elt o 2) all-nodes))))
           all-links))
         (value (completing-read (or prompt "Link: ") collection)))
    (assoc-default value collection #'string=)))

(defun epwgraph-complete-new-logical-link (&optional prompt)
  "Return a list of ((nil src-id dest-id) (nil src-id dest-id) ...)"
  (let* ((sources (epwgraph-complete-logical-node-name "Source: "))
         (dests (epwgraph-complete-logical-node-name "Destination: "))
         (links (epwgraph--get-all-links)))
    (seq-difference
     (epwgraph--map-channels sources dests)
     links
     (lambda (a b)
       (and (= (elt a 1) (elt b 1))
            (= (elt a 2) (elt b 2)))))))

(defun epwgraph--get-channel (s)
  (if (string-match "_\\(.+\\)$" s)
      (match-string 1 s)
    nil))

(defun epwgraph--map-channels (sources dests)
  "Try to match the channels (ex: L or R).
SOURCES is a list of (id . name).
Return a list of links ((id . id) ...).
If we can't match everything exactly, just do it in order."
  (let* ((mapped-dests (mapcar (lambda (o) (cons (epwgraph--get-channel (cdr o)) (car o))) dests))
         (by-channel (seq-keep
                      (lambda (o)
                        (when-let* ((dest-id (assoc-default (epwgraph--get-channel (cdr o))
                                                            mapped-dests)))
                          (list nil (car o) dest-id)))
                      sources)))
    (if (= (length by-channel) (length sources))
        by-channel
      (seq-filter
       (lambda (o) (elt o 2))
       (seq-map-indexed
        (lambda (o i)
          (list nil
                (car o)
                (car (elt dests i))))
        sources)))))

(defun epwgraph-connect-logical-nodes (logical-links)
  "Connect using normalized names."
  (interactive (list (epwgraph-complete-new-logical-link)))
  (with-current-buffer (get-buffer-create "*PipeWire*")
    (dolist (link logical-links)
      (call-process "pw-link" nil t t
                    (number-to-string (elt link 1))
                    (number-to-string (elt link 2)))))
  (epwgraph-refresh))

(defun epwgraph-connect-ports (source dest)
  "Connect SOURCE to DEST specifically."
  (interactive (list (epwgraph-complete-port)
                     (epwgraph-complete-port)))
  (call-process "pw-link" nil nil nil
                (number-to-string (car source))
                (number-to-string (car dest)))
  (epwgraph-refresh))

(defun epwgraph-disconnect-logical-nodes (logical-links)
  (interactive (list (epwgraph-complete-existing-logical-link "Disconnect link: ")))
  (with-current-buffer (get-buffer-create "*PipeWire*")
    (dolist (link logical-links)
      (call-process "pw-link" nil t t "-d" (number-to-string (car link)))))
  (epwgraph-refresh))

(defun epwgraph-disconnect-ports (link)
  (interactive (list (epwgraph-complete-existing-link "Disconnect link: ")))
  (with-current-buffer (get-buffer-create "*PipeWire*")
    (call-process "pw-link" nil t t "-d" (number-to-string (car link))))
  (epwgraph-refresh))

(provide 'epwgraph)
