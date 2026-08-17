;;; annotated-completing-read.el --- ergonomic completing-read wrapper/helper  -*- lexical-binding: t -*-

;; Author: sam kleinman <garen@tychoish.com>
;; Assisted-by: Claude:Sonnet-4.6
;; Maintainer: sam kleinman <garen@tychoish.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, matching
;; URL: https://github.com/tychoish/annotated-completing-read

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides `annotated-completing-read', a wrapper around
;; `completing-read', that accepts a hash table of candidates to
;; annotations and surfaces them as aligned completion metadata
;; understood by vertico, marginalia, and embark.
;;
;; Also provides `annotated-completing-read-context-from-point', a
;; context-aware selection interface, that populates candidates from
;; thing-at-point, the active region, the current line, and the kill
;; ring.

;;; Code:

;; stdlib packages
(require 'cl-lib)
(require 'subr-x)

(require 'map)
(require 'seq)

(require 'project)

(defvar desktop-globals-to-save)
(defvar savehist-additional-variables)

(defvar annotated-completing-read-annotation-face 'default
  "Controls how face properties are applied to annotation strings.

`default': apply `completions-annotations' to annotations that
carry no face text property.  This is the default.

`override': always apply `completions-annotations', overriding
any existing face.

`strip': remove all face text properties from annotations.
Other symbols are treated as a face name and applied to
annotations that carry no face text property.")

(defvar annotated-completing-read-history (make-hash-table :test #'equal)
  "Hash table mapping command symbols to per-command minibuffer history lists.
Keys are symbols, typically `this-command' at call time, and values are
the standard Emacs history lists accumulated by `completing-read'.")

(defun annotated-completing-read-clear-history ()
  "Clear the per-command completion history."
  (interactive)
  (setq annotated-completing-read-history (make-hash-table :test #'equal)))

;;;###autoload
(cl-defun annotated-completing-read
    (table &key (prompt "=> ") require-match category history group-name group-display initial-input sort-fn default or-nil multiple min max)
  "Read a candidate from completion TABLE.
TABLE maps candidates to annotations or target values.
Alignment is automatic.

TABLE can be a hash table or an alist:

- A list-form alist uses the format: =((CANDIDATE ANNOTATION) ...)=.
- A dotted alist uses the format: =((CANDIDATE . ANNOTATION) ...)=.
- A triple-form alist uses the format: =((CANDIDATE ANNOTATION . TARGET) ...)=.
- An annotation can be =nil=.

PROMPT is the minibuffer prompt.  It defaults to ='=> '=.
A trailing space is appended if it is missing.

REQUIRE-MATCH forces the user to select an existing candidate.
If nil, the minibuffer accepts arbitrary input.

CATEGORY is a symbol for the completion category.
External packages like embark or marginalia use it to determine behavior.
Common values include `file', `buffer', `command', and `symbol'.

HISTORY is a symbol representing the history list.
It defaults to `this-command'.
Use a shared symbol to share history between commands.

GROUP-NAME determines candidate grouping.
It can be a function or a static string.

GROUP-DISPLAY formats the candidate text for display.
It is a function that takes a candidate string.
This option requires GROUP-NAME.

INITIAL-INPUT is an optional string to pre-fill in the minibuffer.

SORT-FN is a function to sort candidates before display.

DEFAULT is the fallback return value.
It is returned on empty input or quit.

OR-NIL silences quit and empty input by returning nil.
This option takes effect only when DEFAULT is nil.

A table entry can supply an optional TARGET.
This TARGET is returned instead of the candidate string.
It also affects selection via DEFAULT.
It allows packages like embark to act on the target directly.

MULTIPLE allows selecting multiple candidates.  It returns an ordered
list of selections.  Press `annotated-completing-read--multi-continue' to
accept a pick and continue.  Press
`annotated-completing-read--multi-finish-now' to finish immediately.
Pressing RET accepts the current input and finishes.  DEFAULT and OR-NIL
apply to the entire session.

MIN is the minimum number of required selections.

MAX is the maximum number of allowed selections.

Reaching MAX finishes the session automatically.
These options require MULTIPLE."
  (when (and (or min max) (not multiple))
    (user-error "MIN and MAX require MULTIPLE to be non-nil"))
  (let ((table (annotated-completing-read--to-map table)))
    (when (and (or default or-nil) (zerop (map-length table)))
      (cl-return-from annotated-completing-read
        (if multiple default (annotated-completing-read--resolve-target table default))))
    (if multiple
        (annotated-completing-read--read-multiple
         table prompt require-match category history group-name group-display
         initial-input sort-fn default or-nil min max)
      (let* ((prompt (if (string-suffix-p " " prompt) prompt (concat prompt " ")))
             (hist-key (or history this-command 'annotated-completing-read))
             (collection (annotated-completing-read--build-collection
                          table category group-name group-display sort-fn))
             (hist-sym (make-symbol "history-cell")))
        (annotated-completing-read--ensure-history)
        (set hist-sym (map-elt annotated-completing-read-history hist-key))
        (let ((result (condition-case err
                          (completing-read prompt collection nil require-match initial-input hist-sym default)
                        (quit (cond (default default)
                                    (or-nil nil)
                                    (t (signal (car err) (cdr err))))))))
          (annotated-completing-read--ensure-history)
          (setf (map-elt annotated-completing-read-history hist-key) (symbol-value hist-sym))
          (cond
           ((not (equal result "")) (annotated-completing-read--resolve-target table result))
           (default (annotated-completing-read--resolve-target table default))
           (or-nil nil)
           (t result)))))))

(defun annotated-completing-read--tag-multi-category (table category)
  "Return candidate keys from TABLE.
Candidates with a target value are annotated with CATEGORY.
This allows tools like embark to act on the target directly.
No annotations are added if CATEGORY is nil."
  (if (not category)
      (map-keys table)
    (thread-last (map-keys table)
      (seq-map (lambda (candidate)
                 (let ((value (map-elt table candidate)))
                   (if (consp value)
                       (propertize candidate 'multi-category (cons category (cdr value)))
                     candidate)))))))

(defun annotated-completing-read--build-collection (table category group-name group-display sort-fn)
  "Return a completion COLLECTION function for TABLE.
CATEGORY, GROUP-NAME, GROUP-DISPLAY, and SORT-FN specify metadata.
These arguments have the same meaning as in `annotated-completing-read'."
  (let* ((candidates (annotated-completing-read--tag-multi-category table category))
         (longest (annotated-completing-read--length-of-longest candidates))
         (annotate-fn (lambda (candidate)
                        (when-let* ((value (map-elt table candidate))
                                    (ann (if (consp value) (car value) value)))
                          (annotated-completing-read--apply-annotation-face
                           (concat (annotated-completing-read--prefix-padding candidate longest)
                                   ann)
                           ann))))
         (name-fn (cond ((functionp group-name) group-name)
                        (group-name (lambda (_candidate) group-name))))
         (display-fn (or group-display #'identity))
         (group-fn (when name-fn
                     (lambda (candidate transform)
                       (if transform
                           (funcall display-fn candidate)
                         (funcall name-fn candidate))))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,annotate-fn)
            ,@(when category `((category . ,category)))
            ,@(when group-fn `((group-function . ,group-fn)))
            ,@(when sort-fn `((display-sort-function . ,sort-fn))))
        (complete-with-action action candidates str pred)))))

(defvar-local annotated-completing-read--multi-signal-box nil
  "Mutable one-element list for signaling user actions.
This is set by `annotated-completing-read-multi-mode'
commands to tell the `:multiple' loop in `annotated-completing-read' what the
user just requested.  Car is nil (finish, accepting the current input as the
last pick), `continue' (accept and prompt again), or `discard' (finish now,
ignoring the current input).")

(defvar-keymap annotated-completing-read-multi-mode-map
  :doc "Keymap active in the minibuffer during a `:multiple' ACR session.
Deliberately does not bind `C-.', which stays available for `embark-act' —
see the Design section of the ACR multi-select plan for why."
  "M-," #'annotated-completing-read--multi-continue
  "M-." #'annotated-completing-read--multi-finish-now)

(define-minor-mode annotated-completing-read-multi-mode
  "Minor mode for multi-select completion sessions.
Press `M-,' to accept the current input and continue.
Press `M-.' to finish the session and discard pending input."
  :lighter " ACR-multi"
  :keymap annotated-completing-read-multi-mode-map)

(defun annotated-completing-read--multi-continue ()
  "Accept the current selection and prompt for the next one."
  (interactive)
  (setcar annotated-completing-read--multi-signal-box 'continue)
  (exit-minibuffer))

(defun annotated-completing-read--multi-finish-now ()
  "Finish the session and discard any pending input."
  (interactive)
  (setcar annotated-completing-read--multi-signal-box 'discard)
  (exit-minibuffer))

(defun annotated-completing-read--multi-session
    (signal-box prompt collection require-match initial-input hist)
  "Run a single multi-select completion step.
SIGNAL-BOX receives control signals from the user.
PROMPT is the minibuffer prompt.
COLLECTION is the completion collection.
REQUIRE-MATCH determines whether a match is required.
INITIAL-INPUT is the initial input.
HIST is the history list symbol."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq-local annotated-completing-read--multi-signal-box signal-box)
        (annotated-completing-read-multi-mode 1))
    (completing-read prompt collection nil require-match initial-input hist)))

(defun annotated-completing-read--read-multiple
    (table prompt require-match category history group-name group-display
           initial-input sort-fn default or-nil min max)
  "Read multiple selections from TABLE.
PROMPT is the prompt string.
REQUIRE-MATCH indicates whether match is required.
CATEGORY is the metadata category.
HISTORY is the history symbol.
GROUP-NAME and GROUP-DISPLAY are grouping functions.
INITIAL-INPUT is the initial input.
SORT-FN is the sorting function.
DEFAULT is the fallback value.
OR-NIL controls return on quit.
MIN and MAX define selection limits.
This function returns an ordered list of resolved targets."
  (let* ((prompt (if (string-suffix-p " " prompt) prompt (concat prompt " ")))
         (hist-key (or history this-command 'annotated-completing-read))
         (working-table (copy-hash-table table))
         (picks nil)
         (iteration 0)
         (outcome
          (catch 'annotated-completing-read--multi-done
            (while t
              (when (zerop (map-length working-table))
                (throw 'annotated-completing-read--multi-done 'done))
              (let* ((collection (annotated-completing-read--build-collection
                                  working-table category group-name group-display sort-fn))
                     (session-prompt (format "[%d] %s" (length picks) prompt))
                     (scratch-hist (make-symbol "history-cell"))
                     (signal-box (list nil))
                     (result
                      (condition-case _err
                          (annotated-completing-read--multi-session
                           signal-box session-prompt collection require-match
                           (when (zerop iteration) initial-input) scratch-hist)
                        (quit (throw 'annotated-completing-read--multi-done 'quit)))))
                (cl-incf iteration)
                (let ((discard (eq (car signal-box) 'discard))
                      (continue (eq (car signal-box) 'continue)))
                  (unless (or discard (string-empty-p result))
                    (push (annotated-completing-read--resolve-target working-table result) picks)
                    (remhash result working-table))
                  (cond
                   (continue)
                   ((and min (< (length picks) min))
                    (message "At least %d selection%s required (%d so far)"
                             min (if (= min 1) "" "s") (length picks)))
                   (t (throw 'annotated-completing-read--multi-done 'done)))
                  (when (and max (>= (length picks) max))
                    (throw 'annotated-completing-read--multi-done 'done))))))))
    (setq picks (nreverse picks))
    (annotated-completing-read--ensure-history)
    (setf (map-elt annotated-completing-read-history hist-key) (list picks))
    (pcase outcome
      ('quit (cond (or-nil nil) (default default) (t (signal 'quit nil))))
      (_ picks))))

;;;###autoload
(cl-defun annotated-completing-read-directory
    (&optional &key candidates prompt require-match multiple min max)
  "Select a directory using annotated completion.
CANDIDATES is an optional list of directory paths.
If nil, candidates are gathered from the current context.
PROMPT is the minibuffer prompt.  It defaults to \"directory:\".
REQUIRE-MATCH determines whether a match is required.
MULTIPLE enables selecting multiple directories.
MIN and MAX define selection limits for multiple selections.
Annotations show directory relationships or entry counts."
  (let* ((dirs (or (annotated-completing-read--directory-clean candidates)
                  (annotated-completing-read--directory-default-candidates)))
	(project-root (annotated-completing-read--project-root))
        (relationship (map-into
 		       (thread-last dirs
			 (seq-map #'file-truename)
			 (seq-map (lambda (it)
				    (cons it
					  (cond
					   ((and (equal it project-root) (equal it default-directory)) '("current directory (project root)" . 1))
					   ((equal it project-root) '("project root" . 2))
					   ((equal it default-directory) '("current directory" . 1))
					   ((string-prefix-p it default-directory) '("parent" . 2))
					   ((string-prefix-p default-directory it) '("child" . 5))
					   ((equal (file-name-directory (directory-file-name it))
						   (file-name-directory (directory-file-name default-directory))) '("sibling" . 2))
					   (t '("other" . 10)))))))
		       'hash-table)))

    (if-let* (((> (map-length relationship) 8))
	      (counts (map-into
		       (seq-map (lambda (it) (cons it (annotated-completing-read--directory-entry-counts it))) dirs)
		       'hash-table)))
	;; then
        (annotated-completing-read
	 counts
	 :prompt (or prompt "directory:")
	 :require-match require-match
	 :multiple multiple
	 :min min
	 :max max
	 :group-name (lambda (c) (car (map-elt relationship c '("other" . 10))))
	 :sort-fn (lambda (candidates)
		    (seq-sort-by (lambda (c) (cdr (map-elt relationship c '("other" . 10))))
				 #'< candidates)))

      ;; else
      (annotated-completing-read
       (map-into
        (thread-last dirs
          (seq-map #'file-truename)
          (seq-map (lambda (it)
                     (cons it (concat (car (map-elt relationship it '("other" . 10)))
                                       (annotated-completing-read--directory-buffer-suffix it))))))
        'hash-table)
       :prompt (or prompt "directory:")
       :require-match require-match
       :multiple multiple
       :min min
       :max max
       :sort-fn (lambda (candidates)
		  (seq-sort-by (lambda (c) (cdr (map-elt relationship c '("other" . 10))))
			       #'< candidates))))))

;;;###autoload
(cl-defun annotated-completing-read-context-from-point (&optional &key prompt seed initial-input history)
  "Select a candidate from the current editing context.

PROMPT is the minibuffer prompt.

SEED specifies explicit candidate strings.

INITIAL-INPUT is the initial minibuffer text.

HISTORY specifies the history list.

This function returns an empty string if no candidate is chosen."
  (annotated-completing-read
   (annotated-completing-read--context-candidates seed)
   :require-match nil
   :prompt (or prompt "context:")
   :initial-input initial-input
   :default ""
   :history (or history this-command 'annotated-completing-read-context-from-point)))

(defun annotated-completing-read--length-of-longest (items)
  "Return the length of the longest string in ITEMS."
  (apply #'max 0 (seq-map #'length items)))

(defun annotated-completing-read--prefix-padding (key longest)
  "Return space padding for KEY based on LONGEST key length."
  (make-string (abs (+ 4 (- longest (length key)))) ?\s))

(defun annotated-completing-read--validate-hash-value (value)
  "Validate a hash table entry VALUE.
VALUE must be a string, nil, or a cons cell."
  (unless (or (stringp value) (null value) (consp value))
    (user-error "Hash-table annotation must be a string, nil, or (ANNOTATION . TARGET); got: %S" value)))

(defun annotated-completing-read--normalize-alist-value (value)
  "Normalize an alist VALUE to an annotation or annotation-target pair."
  (cond
   ((and (consp value) (cdr value))
    (cons (car value) (cdr value)))
   ((consp value)
    (car value))
   (t
    value)))

(defun annotated-completing-read--resolve-target (table candidate)
  "Return the target value for CANDIDATE in TABLE.
If no target is found, return CANDIDATE itself."
  (let ((value (map-elt table candidate)))
    (if (consp value)
        (cdr value)
      candidate)))

(defun annotated-completing-read--to-map (table)
  "Convert TABLE into a normalized hash table.
TABLE can be a hash table or an alist.
This function returns a hash table mapping candidates to targets or annotations."
  (cond
   ((hash-table-p table)
    (seq-do #'annotated-completing-read--validate-hash-value (map-values table))
    table)
   ((proper-list-p table)
    (seq-do (lambda (pair)
              (unless (consp pair)
                (user-error "Each alist entry must be a cons cell; got: %S" pair)))
            table)
    (map-into
     (seq-map (lambda (pair)
                (cons (car pair) (annotated-completing-read--normalize-alist-value (cdr pair))))
              table)
     'hash-table))
   (t
    (user-error "TABLE must be a hash table or alist mapping candidates to annotations"))))

(defun annotated-completing-read--ensure-history ()
  "Initialize or fix the global history hash table."
  (cond
    ((hash-table-p annotated-completing-read-history))
    ((and (proper-list-p annotated-completing-read-history)
          (or (null annotated-completing-read-history)
              (seq-every-p #'consp annotated-completing-read-history)))
     (setq annotated-completing-read-history (map-into annotated-completing-read-history 'hash-table)))
    (t
     (setq annotated-completing-read-history (make-hash-table :test #'equal)))))

(defun annotated-completing-read--apply-annotation-face (padded raw)
  "Apply a face to PADDED annotation text.
The face behavior is determined by `annotated-completing-read-annotation-face'.
Existing face properties on RAW are preserved."
  (pcase annotated-completing-read-annotation-face
    ('strip
     (substring-no-properties padded))
    ('override
     (propertize padded 'face 'completions-annotations))
    ('default
     (if (get-text-property 0 'face raw)
         padded
       (propertize padded 'face 'completions-annotations)))
    (face
     (if (get-text-property 0 'face raw)
         padded
       (propertize padded 'face face)))))

(defun annotated-completing-read--context-candidates (&optional seed)
  "Build candidates from the current context.
SEED contains optional extra strings."
  (thread-last
    (append
     ;; current line
     (when-let* ((line (thing-at-point 'line)))
       (list (cons line (format "line · %s" (buffer-name)))))
     ;; seeds
     (thread-last
       (cond
	((listp seed) seed)
	((stringp seed) (list seed)))
       (seq-remove #'null)
       (seq-map (lambda (s) (cons s "seed"))))
     ;; thing-at-point
     (thread-last
       (cond
	((derived-mode-p 'prog-mode) '(symbol word sexp defun))
	((derived-mode-p 'text-mode) '(word email url sentence)))
       (seq-map (lambda (tap) (cons tap (thing-at-point tap))))
       (seq-remove (lambda (pair) (or (null (cdr pair)) (>= (length (cdr pair)) 64))))
       (mapcar (lambda (tapv) (cons (cdr tapv) (format "%s at point" (car tapv))))))
     ;; active region
     (when (use-region-p)
       (list (cons (buffer-substring-no-properties (region-beginning) (region-end))
		   (format "region · %s" (buffer-name)))))
     ;; kill ring — first 10 entries with 1-based index annotations
     (seq-take
      (thread-last
	kill-ring
	(seq-remove #'null)
	(seq-map-indexed (lambda (s i) (cons s (format "kill-ring [%d]" (1+ i))))))
      10))
    ;; normalize: each step is its own stage
    (seq-map (lambda (p) (cons (substring-no-properties (car p)) (cdr p))))
    (seq-map (lambda (p) (cons (string-trim (car p)) (cdr p))))
    (seq-remove (lambda (p) (string-empty-p (car p))))
    (seq-filter (lambda (p) (< (length (car p)) 128)))))


(declare-function projectile-project-buffers "projectile")
(declare-function projectile-project-root "projectile")

(defun annotated-completing-read--project-root ()
  "Return the root directory of the current project."
  (or (when-let* ((project (project-current))) (project-root project))
      (when (featurep 'projectile) (projectile-project-root))
      (expand-file-name default-directory)))

(defun annotated-completing-read--project-buffers ()
  "Return the list of buffers belonging to the current project."
  (or (when-let* ((project (project-current))) (project-buffers project))
      (when (featurep 'projectile) (projectile-project-buffers))
      (let ((dir (annotated-completing-read--project-root)))
	(seq-filter (lambda (it)
		      (with-current-buffer it
			(file-in-directory-p (buffer-file-name it) dir)))
		    (buffer-list)))))

(defun annotated-completing-read--filter-directories (sequence)
  "Filter SEQUENCE to existing directories.
Paths are expanded and deduplicated."
  (thread-last sequence
       (seq-filter #'stringp)
       (seq-map #'string-trim)
       (seq-remove #'string-empty-p)
       (seq-map (lambda (it)
		  (or (when (file-regular-p it)
			(file-name-directory it))
		      it)))
       (seq-map #'expand-file-name)
       (seq-uniq)
       (seq-filter #'file-directory-p)))

(defun annotated-completing-read--directory-clean (dirs)
  "Normalize directory list DIRS.
Empty entries are discarded.
Paths are expanded and deduplicated."
  (thread-last dirs
       (seq-remove #'null)
       (seq-map #'string-trim)
       (seq-remove #'string-empty-p)
       (seq-map #'expand-file-name)
       (seq-map #'directory-file-name)
       (seq-map #'file-truename)
       (seq-uniq)
       (seq-map #'file-name-as-directory)))

(defun annotated-completing-read--directory-parents (&optional start stop)
  "Return directory paths from START up to STOP."
  (let* ((stop-path (expand-file-name (string-trim (or stop "~/"))))
         (current (expand-file-name (string-trim (or start default-directory))))
         (output (list stop-path current)))
    (while (and current (not (string= current stop-path)))
      (setq current (file-name-parent-directory current))
      (push current output))
    (annotated-completing-read--filter-directories output)))

(defun annotated-completing-read--directory-default-candidates ()
  "Return default directory candidates from the current context."
  (let* ((proj-root (annotated-completing-read--project-root))
	 (home (expand-file-name "~/"))
	 (candidates
	  (append
	   ;; includes all paths between the current directory and the
	   ;; project root (inclusive)
	   (annotated-completing-read--directory-parents default-directory proj-root)
	   ;; includes the directory of every path that has an open buffer.
	   (thread-last
	     (annotated-completing-read--project-buffers)
	     (seq-map #'buffer-file-name)
	     (seq-remove #'null)
	     ;; NOTE: if we have a buffer that's file name is the
	     ;; project root itself then this puts the parent of the
	     ;; project root (which the previous item should include)
	     ;; we run distinct at the end too, so it's fine
	     (seq-keep #'file-name-directory)
	     (seq-uniq))
	   ;; a collection of things that __might__ be something the
	   ;; user is trying for guess
	   (list
	    (thing-at-point 'filename)
	    (thing-at-point 'existing-filename)
	    default-directory
	    proj-root
	    home))))

    (annotated-completing-read--filter-directories
     ;; if the list is relatively short, add all of the top level
     ;; directories in the project root
     ;; do one big filter pass to make sure we only give
     ;; directories, and things get expanded correctly:
     (if (or (and (length< candidates 16) (not (string-equal home proj-root)))
             current-prefix-arg)
         (nconc (seq-filter #'file-directory-p (directory-files proj-root t "[^\\.]")) candidates)
       candidates))))

(defun annotated-completing-read--directory-buffer-count (dir)
  "Return the number of open buffers visiting files under DIR."
  (let ((prefix (file-name-as-directory (expand-file-name dir))))
    (thread-last (buffer-list)
      (seq-map #'buffer-file-name)
      (seq-remove #'null)
      (seq-count (lambda (file) (string-prefix-p prefix (expand-file-name file)))))))

(defun annotated-completing-read--directory-buffer-suffix (dir)
  "Return a string describing open buffers under DIR.
It returns an empty string if no buffers are open."
  (let ((buffers (annotated-completing-read--directory-buffer-count dir)))
    (if (> buffers 0)
        (format ", %d buffer%s" buffers (if (= buffers 1) "" "s"))
      "")))

(defun annotated-completing-read--directory-entry-counts (dir)
  "Return an entry count string for DIR."
  (concat
   (or (when (file-accessible-directory-p dir)
	 (let* ((entries (directory-files dir t "\\`[^.]"))
                (n-dirs (seq-count #'file-directory-p entries))
                (n-files (- (length entries) n-dirs)))
           (format "%d dirs, %d files" n-dirs n-files)))
       "")
   (annotated-completing-read--directory-buffer-suffix dir)))

;;;###autoload
(defun annotated-completing-read-setup-history ()
  "Enable savehist and desktop history integration for `annotated-completing-read'."
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-globals-to-save 'annotated-completing-read-history))
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'annotated-completing-read-history)
    (add-hook 'savehist-mode-hook #'annotated-completing-read--ensure-history)))

(provide 'annotated-completing-read)
;;; annotated-completing-read.el ends here
