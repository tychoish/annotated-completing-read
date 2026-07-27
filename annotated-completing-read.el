;;; annotated-completing-read.el --- Completing-read with aligned annotations -*- lexical-binding: t -*-

;; Author: sam kleinman
;; Assisted-by: Claude:Sonnet-4.6
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, matching
;; URL: https://github.com/tychoish/dot-emacs

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

(defvar annotated-completing-read-annotation-face 'default
  "Controls how face properties are applied to annotation strings.

`default'  — apply `completions-annotations' to annotations that carry no
             face text property.  This is the default.
`override' — always apply `completions-annotations', overriding any existing face.
`strip'    — remove all face text properties from annotations.
Any other symbol — treat it as a face name and apply it to annotations that
             carry no face text property.")

(defvar annotated-completing-read-history (make-hash-table :test #'equal)
  "Hash table mapping command symbols to per-command minibuffer history lists.
Keys are symbols — typically `this-command' at call time — and values are
the standard Emacs history lists accumulated by `completing-read'.")

(defun annotated-completing-read-clear-history ()
  "Reset the annotated-completing-read per-command history to an empty state."
  (interactive)
  (setq annotated-completing-read-history (make-hash-table :test #'equal)))

;;;###autoload
(cl-defun annotated-completing-read
    (table &key (prompt "=> ") require-match category history group-name group-display initial-input sort-fn default or-nil multiple min max)
  "Read a candidate from TABLE with aligned per-candidate annotations.
TABLE is any Emacs hash table `make-hash-table' mapping candidate
strings to annotation strings.  Column alignment is computed
automatically; callers need not pad candidates or annotations.

PROMPT is the minibuffer prompt (default \"=> \"); a trailing space is appended
automatically if absent.

REQUIRE-MATCH, when non-nil, forces the user to select an existing candidate.
When nil (the default), arbitrary input not present in TABLE is accepted and
returned verbatim.

CATEGORY is an optional completion-category symbol surfaced as table metadata.
Completion UIs (vertico, embark, marginalia) use it to select annotations,
keybindings, and actions.  Common values:

  `file'              – file-name actions, path display via marginalia
  `buffer'            – buffer switching and embark buffer actions
  `command'           – executed-extended-command dispatch
  `symbol'            – Lisp symbol lookup and eldoc integration
  `bookmark'          – `bookmark-jump' actions
  `consult-grep'      – `consult-grep' result actions (jump to line, etc.)
  `consult-mu'        – `consult-mu' mail account entries

HISTORY is a symbol key into `annotated-completing-read-history' (a hash table
of per-command history lists).  Defaults to `this-command' captured at call
time, giving each command its own isolated history automatically.  Pass an
explicit symbol to share history across several call sites.

GROUP-NAME is a function (CANDIDATE) => group-name-string that returns which
group a candidate belongs to, or a plain string for a single constant group.
When nil, no grouping metadata is emitted.

GROUP-DISPLAY is an optional function (CANDIDATE) => display-string that
controls how a candidate is rendered within its group.  Defaults to identity
where candidate are displayed verbatim.  Only meaningful when GROUP-NAME is set.

Together GROUP-NAME and GROUP-DISPLAY are assembled into the `group-function'
completion metadata entry expected by vertico and other UIs.

INITIAL-INPUT is an optional string pre-filled into the minibuffer.

SORT-FN is an optional function (LIST-OF-STRINGS) => LIST-OF-STRINGS that
reorders candidates before display.  Surfaced as `display-sort-function' in
completion metadata, so vertico and other UIs apply it before rendering.

DEFAULT is a string returned when TABLE is empty (no prompt is shown), the
user accepts empty input, or the user quits with \\[keyboard-quit].  When
non-nil it is also passed to `completing-read' as its DEF argument so
completion UIs can display it in the prompt.

OR-NIL, when non-nil, silences \\[keyboard-quit] and empty input by returning
nil instead.  Useful when the caller treats nil as \"nothing selected\" without
needing a specific fallback string.  Takes effect only when DEFAULT is nil;
DEFAULT takes precedence.

TABLE may be a hash table, a dotted alist ((CANDIDATE . ANNOTATION) ...), a
list-form alist ((CANDIDATE ANNOTATION) ...), or a triple-form alist
\((CANDIDATE ANNOTATION . TARGET) ...).  ANNOTATION may be nil to suppress
the annotation for that candidate.  Signals `user-error' for any other type.

TARGET, when a table entry supplies one (via the triple-form list entry or
a hash-table value of (ANNOTATION . TARGET)), is returned in place of the
selected candidate string — including when the candidate is resolved via
DEFAULT.  Entries with no target continue to return the candidate string,
unchanged from prior behavior.  When TARGET is present, its candidate string
is also tagged with a `multi-category' text property so embark acts on
TARGET rather than the display string; see
`annotated-completing-read--tag-multi-category'.

MULTIPLE, when non-nil, returns an ordered list of picks instead of a single
string/target.  The session stays in one minibuffer prompt per pick, driven
by `annotated-completing-read-multi-mode': \\<annotated-completing-read-multi-mode-map>\
\\[annotated-completing-read--multi-continue] accepts the current input as a
pick and prompts again with it excluded from TABLE;
\\[annotated-completing-read--multi-finish-now] finishes immediately,
discarding unaccepted pending input; plain RET accepts the current input (if
any) as a final pick and finishes, matching the single-pick behavior of a
`:multiple t' call with exactly one pick.  DEFAULT/OR-NIL apply to the whole
call on quit or on an empty table, not per pick.

MIN and MAX are only meaningful with MULTIPLE, and signal `user-error'
otherwise.  MIN is the fewest picks required before RET/
\\[annotated-completing-read--multi-finish-now] are honored; below it, the
session keeps prompting.  MAX is a cap: reaching it ends the session
immediately, as if \\[annotated-completing-read--multi-finish-now] had been
pressed."
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
  "Return TABLE's candidate keys, propertizing TARGET-bearing ones.
Each candidate whose TABLE value is a cons (ANNOTATION . TARGET) is
propertized with a `multi-category' text property of (CATEGORY . TARGET) —
the mechanism embark already ships a transformer for in
`embark-transformer-alist', so embark resolves TARGET with no further setup.
Candidates are left unpropertized when CATEGORY is nil, since there is then
no type for embark to dispatch on."
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
CATEGORY, GROUP-NAME, GROUP-DISPLAY, and SORT-FN have the same meaning as in
`annotated-completing-read'.  Shared by the single-pick and `:multiple' code
paths so annotation/grouping/sorting behave identically in both; the
`:multiple' loop calls this again each iteration so it recomputes from
whatever TABLE remains after excluding prior picks."
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
  "Mutable one-element list set by `annotated-completing-read-multi-mode'
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
  "Minor mode enabling `M-,'/`M-.' for a `:multiple' ACR session.
`M-,' accepts the current input as a pick and continues the session;
`M-.' finishes the session now, discarding any unaccepted pending input."
  :lighter " ACR-multi"
  :keymap annotated-completing-read-multi-mode-map)

(defun annotated-completing-read--multi-continue ()
  "Accept the current minibuffer input as a pick and continue the session."
  (interactive)
  (setcar annotated-completing-read--multi-signal-box 'continue)
  (exit-minibuffer))

(defun annotated-completing-read--multi-finish-now ()
  "Finish the `:multiple' session now, discarding unaccepted pending input."
  (interactive)
  (setcar annotated-completing-read--multi-signal-box 'discard)
  (exit-minibuffer))

(defun annotated-completing-read--multi-session
    (signal-box prompt collection require-match initial-input hist)
  "Run one `completing-read' call with `annotated-completing-read-multi-mode'.
SIGNAL-BOX is the mutable box `annotated-completing-read--multi-continue'
and `annotated-completing-read--multi-finish-now' set; the other arguments
are `completing-read' arguments, forwarded unchanged.  A plain function
\(not `minibuffer-with-setup-hook' inlined at the call site) so tests can
replace it with a mock instead of trying to override a macro's already-
expanded call site."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq-local annotated-completing-read--multi-signal-box signal-box)
        (annotated-completing-read-multi-mode 1))
    (completing-read prompt collection nil require-match initial-input hist)))

(defun annotated-completing-read--read-multiple
    (table prompt require-match category history group-name group-display
           initial-input sort-fn default or-nil min max)
  "Run the `:multiple' loop for `annotated-completing-read' against TABLE.
Returns an ordered list of resolved picks.  See `annotated-completing-read'
for the meaning of every parameter, and `annotated-completing-read-multi-mode'
for the `M-,'/`M-.' bindings that drive the loop.  Covered by direct ERT
tests in test-annotated-completing-read.el, so kept as its own function
despite having exactly one call site."
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
  "Select a directory with annotated completion.
CANDIDATES is an explicit list of directory paths; if nil, a context-aware
list is computed from the project root, open buffers, and `thing-at-point'.
PROMPT defaults to \"directory: \".  REQUIRE-MATCH, MULTIPLE, MIN, and MAX
are passed through to `annotated-completing-read' unchanged — see its
docstring for what MULTIPLE/MIN/MAX do.

With 8 or fewer candidates the annotation shows the directory's relationship
to the current directory (\"parent\", \"project root\", etc.).  With more
than 8 candidates candidates are grouped by that relationship label and the
annotation shows entry counts instead."
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
  "Select a string from context-aware candidates with PROMPT.
Candidates are drawn from `thing-at-point', the active region, the current
line, the kill ring, and any explicit SEED strings.  SEED may be a
string or a list of strings.  Callers can specify INITIAL-INPUT to
control an initial selection.

HISTORY is a symbol passed to `annotated-completing-read' to scope the
per-command history; defaults to `this-command', giving each calling
command its own isolated history.

Returns the emptry string if there are no options or no selections."
  (annotated-completing-read
   (annotated-completing-read--context-candidates seed)
   :require-match nil
   :prompt (or prompt "context:")
   :initial-input initial-input
   :default ""
   :history (or history this-command 'annotated-completing-read-context-from-point)))

(defun annotated-completing-read--length-of-longest (items)
  (apply #'max 0 (seq-map #'length items)))

(defun annotated-completing-read--prefix-padding (key longest)
  (make-string (abs (+ 4 (- longest (length key)))) ?\s))

(defun annotated-completing-read--validate-hash-value (value)
  "Signal `user-error' unless VALUE is a valid hash-table candidate value.
VALUE must be a string or nil (a plain annotation, no target) or a cons
\(ANNOTATION . TARGET).  Any other shape is rejected, since a hash-table
annotation cannot otherwise be told apart from a target pair."
  (unless (or (stringp value) (null value) (consp value))
    (user-error "Hash-table annotation must be a string, nil, or (ANNOTATION . TARGET); got: %S" value)))

(defun annotated-completing-read--normalize-alist-value (value)
  "Normalize an alist entry VALUE to ANNOTATION or (ANNOTATION . TARGET).
VALUE may be a plain annotation (a string or nil, from a dotted alist
entry), a single-element list (ANNOTATION) (from a list-form alist
entry), or (ANNOTATION . TARGET) (from a triple-form entry).  The result
uses the same shape as a hash-table value: a bare annotation means no
target; a cons carries an explicit target in its cdr."
  (cond
   ((and (consp value) (cdr value))
    (cons (car value) (cdr value)))
   ((consp value)
    (car value))
   (t
    value)))

(defun annotated-completing-read--resolve-target (table candidate)
  "Resolve CANDIDATE's return value from TABLE.
When TABLE's value for CANDIDATE is a cons, its cdr is the target and is
returned.  Otherwise CANDIDATE itself is returned — whether because it has
no entry in TABLE, or because its entry carries only an annotation."
  (let ((value (map-elt table candidate)))
    (if (consp value)
        (cdr value)
      candidate)))

(defun annotated-completing-read--to-map (table)
  "Normalize TABLE to a hash table of candidate -> ANNOTATION-or-TARGET-pair.
TABLE may be a hash table mapping candidates to annotation strings (or to
\(ANNOTATION . TARGET) conses), a dotted alist ((CANDIDATE . ANNOTATION) ...),
a list-form alist ((CANDIDATE ANNOTATION) ...), or a triple-form alist
\((CANDIDATE ANNOTATION . TARGET) ...).  ANNOTATION may be nil to suppress
the annotation for that candidate.  TARGET, when present, is returned by
`annotated-completing-read' in place of the candidate string.

A hash-table TABLE is validated and returned as-is — never copied — since
its values already use the same ANNOTATION-or-(ANNOTATION . TARGET) shape
this function would otherwise produce.  Signals `user-error' for any other
input type."
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
  "Ensure history is a valid hash table.
If savehist or desktop restored the value as an alist, convert it.
Any other non-hash-table value is discarded and replaced with an empty table."
  (cond
    ((hash-table-p annotated-completing-read-history))
    ((and (proper-list-p annotated-completing-read-history)
          (or (null annotated-completing-read-history)
              (seq-every-p #'consp annotated-completing-read-history)))
     (setq annotated-completing-read-history (map-into annotated-completing-read-history 'hash-table)))
    (t
     (setq annotated-completing-read-history (make-hash-table :test #'equal)))))

(defun annotated-completing-read--apply-annotation-face (padded raw)
  "Apply a face to PADDED annotation per `annotated-completing-read-annotation-face'.
RAW is the annotation before padding; its first character is checked for an
existing `face' property to decide whether to apply or skip."
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
  "Build an annotated alist of candidates from the current context.
SEED is a string or list of strings to include as explicit candidates."
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
  (or (when-let* ((project (project-current))) (project-root project))
      (when (featurep 'projectile) (projectile-project-root))
      (expand-file-name default-directory)))

(defun annotated-completing-read--project-buffers ()
  (or (when-let* ((project (project-current))) (project-buffers project))
      (when (featurep 'projectile) (projectile-project-buffers))
      (let ((dir (annotated-completing-read--project-root)))
	(seq-filter (lambda (it)
		      (with-current-buffer it
			(file-in-directory-p (buffer-file-name it) dir)))
		    (buffer-list)))))

(defun annotated-completing-read--filter-directories (sequence)
  "Return SEQUENCE filtered to existing directories, canonicalized and deduplicated."
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
  "Normalize DIRS: expand relative paths, drop nil/blank, and de-duplicate."
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
  "Return intermediate directory paths walking up from START to STOP."
  (let* ((stop-path (expand-file-name (string-trim (or stop "~/"))))
         (current (expand-file-name (string-trim (or start default-directory))))
         (output (list stop-path current)))
    (while (and current (not (string= current stop-path)))
      (setq current (file-name-parent-directory current))
      (push current output))
    (annotated-completing-read--filter-directories output)))

(defun annotated-completing-read--directory-default-candidates ()
  "Assemble context-aware directory candidates from project, buffers, and point."
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
  "Return the number of live buffers visiting a file under DIR."
  (let ((prefix (file-name-as-directory (expand-file-name dir))))
    (thread-last (buffer-list)
      (seq-map #'buffer-file-name)
      (seq-remove #'null)
      (seq-count (lambda (file) (string-prefix-p prefix (expand-file-name file)))))))

(defun annotated-completing-read--directory-buffer-suffix (dir)
  "Return \", N buffers\" for DIR when it has open buffers, else \"\"."
  (let ((buffers (annotated-completing-read--directory-buffer-count dir)))
    (if (> buffers 0)
        (format ", %d buffer%s" buffers (if (= buffers 1) "" "s"))
      "")))

(defun annotated-completing-read--directory-entry-counts (dir)
  "Return a brief annotation with subdirectory, file, and buffer counts for DIR."
  (concat
   (or (when (file-accessible-directory-p dir)
	 (let* ((entries (directory-files dir t "\\`[^.]"))
                (n-dirs (seq-count #'file-directory-p entries))
                (n-files (- (length entries) n-dirs)))
           (format "%d dirs, %d files" n-dirs n-files)))
       "")
   (annotated-completing-read--directory-buffer-suffix dir)))

(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'annotated-completing-read-history))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'annotated-completing-read-history)
  (add-hook 'savehist-mode-hook #'annotated-completing-read--ensure-history))

(provide 'annotated-completing-read)
;;; annotated-completing-read.el ends here
