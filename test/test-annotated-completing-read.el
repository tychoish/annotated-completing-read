;;; test-annotated-completing-read.el --- ERT tests for annotated-completing-read -*- lexical-binding: t -*-

;; These tests are designed to run inside a live Emacs session with the full
;; config loaded (M-x ert RET t RET), or via:
;;   (ert-run-tests-batch-and-exit "annotated-completing-read")

(require 'ert)
(require 'map)
(require 'annotated-completing-read)

;;; Helpers

(defmacro acr-with-mock (table return-value &rest body)
  "Call `annotated-completing-read' on TABLE with RETURN-VALUE as mock result.
Within BODY, `captured-args' is bound to the argument list that
`completing-read' was called with, and `captured-collection' is its second
element (the completion table function)."
  (declare (indent 2))
  `(let (captured-args captured-collection)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (&rest args)
                  (setq captured-args args
                        captured-collection (nth 1 args))
                  ,return-value)))
       ,@body)))

(defun acr-metadata (collection)
  "Return the metadata alist from a completion COLLECTION function."
  (cdr (funcall collection "" nil 'metadata)))

(defmacro acr-test--ht (&rest pairs)
  "Create a hash table with equal test from PAIRS of (key value) forms."
  (let ((h (make-symbol "h")))
    `(let ((,h (make-hash-table :test #'equal)))
       ,@(mapcar (lambda (pair) `(map-put! ,h ,(car pair) ,(cadr pair))) pairs)
       ,h)))

;;; Guard

(ert-deftest annotated-completing-read/rejects-non-table-non-alist ()
  (should-error (annotated-completing-read "string") :type 'user-error)
  (should-error (annotated-completing-read [vec])    :type 'user-error)
  (should-error (annotated-completing-read 42)       :type 'user-error))

(ert-deftest annotated-completing-read/accepts-plain-hash-table ()
  (let ((table (make-hash-table :test #'equal)))
    (puthash "foo" "bar" table)
    (acr-with-mock table "foo"
      (should (equal "foo" (annotated-completing-read table))))))

(ert-deftest annotated-completing-read/accepts-hash-table-equal-test ()
  (let ((table (acr-test--ht ("foo" "bar"))))
    (acr-with-mock table "foo"
      (should (equal "foo" (annotated-completing-read table))))))

;;; Prompt normalisation

(ert-deftest annotated-completing-read/prompt-trailing-space-added ()
  (let ((table (acr-test--ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :prompt "Select")
      (should (equal "Select " (nth 0 captured-args))))))

(ert-deftest annotated-completing-read/prompt-trailing-space-not-doubled ()
  (let ((table (acr-test--ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :prompt "Select ")
      (should (equal "Select " (nth 0 captured-args))))))

;;; History

(ert-deftest annotated-completing-read/history-keyed-by-this-command ()
  (let ((annotated-completing-read-history (make-hash-table :test #'equal))
        (this-command 'my-test-command)
        (table (acr-test--ht ("x" "note"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _req _init hist &rest _)
                 (push "x" (symbol-value hist))
                 "x")))
      (annotated-completing-read table))
    (should (equal '("x")
                   (map-elt annotated-completing-read-history 'my-test-command)))))

(ert-deftest annotated-completing-read/explicit-history-key-isolates ()
  (let ((annotated-completing-read-history (make-hash-table :test #'equal))
        (this-command 'other-command)
        (table (acr-test--ht ("x" "note"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _req _init hist &rest _)
                 (push "x" (symbol-value hist))
                 "x")))
      (annotated-completing-read table :history 'explicit-key))
    (should (equal '("x")
                   (map-elt annotated-completing-read-history 'explicit-key)))
    (should (null (map-elt annotated-completing-read-history 'other-command)))))

(ert-deftest annotated-completing-read/history-accumulates-across-calls ()
  (let ((annotated-completing-read-history (make-hash-table :test #'equal))
        (this-command 'accumulate-cmd)
        (table (acr-test--ht ("x" "1") ("y" "2"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _req _init hist &rest _)
                 (let ((val (if (null (symbol-value hist)) "x" "y")))
                   (push val (symbol-value hist))
                   val))))
      (annotated-completing-read table)
      (annotated-completing-read table))
    (should (equal '("y" "x")
                   (map-elt annotated-completing-read-history 'accumulate-cmd)))))

;;; Completion metadata — annotation

(ert-deftest annotated-completing-read/annotation-function-present ()
  (let ((table (acr-test--ht ("alpha" "first letter") ("beta" "second letter"))))
    (acr-with-mock table "alpha"
      (annotated-completing-read table)
      (let ((annotate (alist-get 'annotation-function (acr-metadata captured-collection))))
        (should (functionp annotate))
        (should (string-match-p "first letter"  (funcall annotate "alpha")))
        (should (string-match-p "second letter" (funcall annotate "beta")))))))

(ert-deftest annotated-completing-read/annotation-alignment ()
  "Padding ensures the annotation column position is constant across candidates.
Annotation values must be the same length for a total-width comparison to hold;
the invariant being tested is that key+padding is constant, not key+padding+value."
  (let ((table (acr-test--ht ("a" "x") ("much-longer-key" "y"))))
    (acr-with-mock table "a"
      (annotated-completing-read table)
      (let* ((annotate  (alist-get 'annotation-function (acr-metadata captured-collection)))
             (ann-short (funcall annotate "a"))
             (ann-long  (funcall annotate "much-longer-key")))
        (should (= (+ (length "a")               (length ann-short))
                   (+ (length "much-longer-key") (length ann-long))))))))

;;; Completion metadata — category

(ert-deftest annotated-completing-read/category-surfaced-in-metadata ()
  (let ((table (acr-test--ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :category 'my-category)
      (should (eq 'my-category
                  (alist-get 'category (acr-metadata captured-collection)))))))

(ert-deftest annotated-completing-read/no-category-when-omitted ()
  (let ((table (acr-test--ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table)
      (should (null (alist-get 'category (acr-metadata captured-collection)))))))

;;; Completion metadata — group

(ert-deftest annotated-completing-read/no-group-fn-without-group-name ()
  (let ((table (acr-test--ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table)
      (should (null (alist-get 'group-function (acr-metadata captured-collection)))))))

(ert-deftest annotated-completing-read/group-name-string-constant ()
  (let ((table (acr-test--ht ("a" "ann") ("b" "ann2"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :group-name "My Group")
      (let ((gfn (alist-get 'group-function (acr-metadata captured-collection))))
        (should (functionp gfn))
        (should (equal "My Group" (funcall gfn "a" nil)))
        (should (equal "My Group" (funcall gfn "b" nil)))))))

(ert-deftest annotated-completing-read/group-name-function ()
  (let ((table (acr-test--ht ("TestFoo" "t") ("BenchBar" "b"))))
    (acr-with-mock table "TestFoo"
      (annotated-completing-read
       table
       :group-name (lambda (c) (if (string-prefix-p "Bench" c) "Benchmarks" "Tests")))
      (let ((gfn (alist-get 'group-function (acr-metadata captured-collection))))
        (should (equal "Tests"      (funcall gfn "TestFoo"  nil)))
        (should (equal "Benchmarks" (funcall gfn "BenchBar" nil)))))))

(ert-deftest annotated-completing-read/group-display-defaults-to-identity ()
  (let ((table (acr-test--ht ("TestFoo" "t"))))
    (acr-with-mock table "TestFoo"
      (annotated-completing-read table :group-name "Tests")
      (let ((gfn (alist-get 'group-function (acr-metadata captured-collection))))
        (should (equal "TestFoo" (funcall gfn "TestFoo" t)))))))

(ert-deftest annotated-completing-read/group-display-function ()
  (let ((table (acr-test--ht ("TestFoo" "t") ("TestBar" "b"))))
    (acr-with-mock table "TestFoo"
      (annotated-completing-read
       table
       :group-name    "Tests"
       :group-display (lambda (c) (string-remove-prefix "Test" c)))
      (let ((gfn (alist-get 'group-function (acr-metadata captured-collection))))
        (should (equal "Tests" (funcall gfn "TestFoo" nil)))
        (should (equal "Foo"   (funcall gfn "TestFoo" t)))
        (should (equal "Bar"   (funcall gfn "TestBar" t)))))))

(ert-deftest annotated-completing-read/group-display-without-group-name-ignored ()
  "group-display alone does not produce a group-function."
  (let ((table (acr-test--ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :group-display #'upcase)
      (should (null (alist-get 'group-function (acr-metadata captured-collection)))))))

;;; sort-fn / display-sort-function

(ert-deftest annotated-completing-read/sort-fn-absent-by-default ()
  "`display-sort-function' is not in metadata when :sort-fn is omitted."
  (let ((table (acr-test--ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table)
      (should (null (alist-get 'display-sort-function (acr-metadata captured-collection)))))))

(ert-deftest annotated-completing-read/sort-fn-surfaces-as-display-sort-function ()
  "The function passed as :sort-fn appears as `display-sort-function' in metadata."
  (let* ((table (acr-test--ht ("a" "ann")))
         (my-sort (lambda (items) (sort (copy-sequence items) #'string<))))
    (acr-with-mock table "a"
      (annotated-completing-read table :sort-fn my-sort)
      (should (eq my-sort
                  (alist-get 'display-sort-function (acr-metadata captured-collection)))))))

(ert-deftest annotated-completing-read/sort-fn-reorders-candidates ()
  "Calling the `display-sort-function' from metadata applies the sort."
  (let* ((table (acr-test--ht ("z" "last") ("a" "first")))
         (my-sort (lambda (items) (sort (copy-sequence items) #'string<))))
    (acr-with-mock table "a"
      (annotated-completing-read table :sort-fn my-sort)
      (let* ((dsf (alist-get 'display-sort-function (acr-metadata captured-collection)))
             (result (funcall dsf '("z" "a"))))
        (should (equal '("a" "z") result))))))

;;; require-match / arbitrary input

(ert-deftest annotated-completing-read/arbitrary-input-returned-verbatim ()
  "When require-match is nil, input not in TABLE is returned as-is."
  (let ((table (acr-test--ht ("known" "ann"))))
    (acr-with-mock table "totally-new"
      (should (equal "totally-new"
                     (annotated-completing-read table :require-match nil))))))

(ert-deftest annotated-completing-read/require-match-passed-through ()
  "The require-match value reaches completing-read unchanged."
  (let ((table (acr-test--ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :require-match t)
      (should (eq t (nth 3 captured-args))))
    (acr-with-mock table "a"
      (annotated-completing-read table :require-match nil)
      (should (null (nth 3 captured-args))))))

;;; initial-input

(ert-deftest annotated-completing-read/initial-input-passed-through ()
  (let ((table (acr-test--ht ("foo" "bar"))))
    (acr-with-mock table "foo"
      (annotated-completing-read table :initial-input "fo")
      (should (equal "fo" (nth 4 captured-args))))))

(ert-deftest annotated-completing-read/initial-input-nil-by-default ()
  (let ((table (acr-test--ht ("foo" "bar"))))
    (acr-with-mock table "foo"
      (annotated-completing-read table)
      (should (null (nth 4 captured-args))))))

;;; Collection completions (not just metadata)

(ert-deftest annotated-completing-read/collection-returns-candidates ()
  (let ((table (acr-test--ht ("alpha" "1") ("beta" "2") ("gamma" "3"))))
    (acr-with-mock table "alpha"
      (annotated-completing-read table)
      (let ((all (funcall captured-collection "" nil t)))
        (should (member "alpha" all))
        (should (member "beta"  all))
        (should (member "gamma" all))))))

(ert-deftest annotated-completing-read/collection-filters-by-prefix ()
  (let ((table (acr-test--ht ("alpha" "1") ("beta" "2") ("aleph" "3"))))
    (acr-with-mock table "alpha"
      (annotated-completing-read table)
      (let ((matches (funcall captured-collection "al" nil t)))
        (should (member "alpha" matches))
        (should (member "aleph" matches))
        (should-not (member "beta" matches))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--context-candidates: return type and seeds

(ert-deftest annotated-completing-read/candidates-returns-alist ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (should (proper-list-p (annotated-completing-read--context-candidates))))))

(ert-deftest annotated-completing-read/candidates-seed-string-included ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (should (map-contains-key (annotated-completing-read--context-candidates "hello") "hello")))))

(ert-deftest annotated-completing-read/candidates-seed-annotation ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (should (equal "seed" (map-elt (annotated-completing-read--context-candidates "hello") "hello"))))))

(ert-deftest annotated-completing-read/candidates-seed-list ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (let ((tbl (annotated-completing-read--context-candidates '("foo" "bar"))))
        (should (map-contains-key tbl "foo"))
        (should (map-contains-key tbl "bar"))))))

(ert-deftest annotated-completing-read/candidates-seed-too-long-excluded ()
  "Seeds >= 128 characters are excluded."
  (let ((kill-ring nil)
        (long-seed (make-string 130 ?x)))
    (with-temp-buffer
      (should-not (map-contains-key (annotated-completing-read--context-candidates long-seed) long-seed)))))

(ert-deftest annotated-completing-read/candidates-empty-seed-excluded ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (should-not (map-contains-key (annotated-completing-read--context-candidates "") "")))))

(ert-deftest annotated-completing-read/candidates-whitespace-seed-excluded ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (let ((tbl (annotated-completing-read--context-candidates "   ")))
        (should-not (map-contains-key tbl ""))
        (should-not (map-contains-key tbl "   "))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--context-candidates: kill ring

(ert-deftest annotated-completing-read/candidates-kill-ring-included ()
  (let ((kill-ring (list "from-kill-ring")))
    (with-temp-buffer
      (should (map-contains-key (annotated-completing-read--context-candidates) "from-kill-ring")))))

(ert-deftest annotated-completing-read/candidates-kill-ring-annotation-format ()
  "Kill ring entries are annotated as kill-ring [N] starting at index 1."
  (let ((kill-ring (list "first-item")))
    (with-temp-buffer
      (should (equal "kill-ring [1]" (map-elt (annotated-completing-read--context-candidates) "first-item"))))))

(ert-deftest annotated-completing-read/candidates-kill-ring-limited-to-ten ()
  "At most 10 kill ring entries are included."
  (let ((kill-ring (mapcar (lambda (n) (format "kill-%02d" n)) (number-sequence 1 15))))
    (with-temp-buffer
      (let* ((tbl (annotated-completing-read--context-candidates))
             (kill-keys (seq-filter (lambda (k) (string-prefix-p "kill-" k))
                                    (map-keys tbl))))
        (should (<= (length kill-keys) 10))))))

(ert-deftest annotated-completing-read/candidates-kill-ring-long-item-excluded ()
  "Kill ring items >= 128 characters are excluded."
  (let ((kill-ring (list (make-string 130 ?k))))
    (with-temp-buffer
      (should (= 0 (map-length (annotated-completing-read--context-candidates)))))))

(ert-deftest annotated-completing-read/candidates-kill-ring-whitespace-excluded ()
  (let ((kill-ring (list "   " "\t\n")))
    (with-temp-buffer
      (should (= 0 (map-length (annotated-completing-read--context-candidates)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--context-candidates: region

(ert-deftest annotated-completing-read/candidates-region-included ()
  "Active region content is included as a candidate."
  (let ((kill-ring nil))
    (with-temp-buffer
      (insert "selected text")
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 1))
                ((symbol-function 'region-end) (lambda () (point-max))))
        (should (map-contains-key (annotated-completing-read--context-candidates) "selected text"))))))

(ert-deftest annotated-completing-read/candidates-region-annotation-format ()
  "Region annotation contains both 'region' and the buffer name."
  (let ((kill-ring nil))
    (with-temp-buffer
      (rename-buffer "my-test-buf" t)
      ;; Insert longer line so line candidate != region candidate, preventing overwrite.
      (insert "prefix region content suffix")
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 8))
                ((symbol-function 'region-end) (lambda () 22)))
        ;; positions 8–22 = "region content" (14 chars)
        (let ((annotation (map-elt (annotated-completing-read--context-candidates) "region content")))
          (should (string-match-p "region" annotation))
          (should (string-match-p "my-test-buf" annotation)))))))

(ert-deftest annotated-completing-read/candidates-region-excluded-when-inactive ()
  "When use-region-p is nil no region candidate is added."
  (let ((kill-ring nil))
    (with-temp-buffer
      (insert "some text")
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
        (let* ((tbl (annotated-completing-read--context-candidates))
               (annotation (map-elt tbl "some text")))
          (should-not (and annotation (string-match-p "region" annotation))))))))

(ert-deftest annotated-completing-read/candidates-region-too-long-excluded ()
  "Region content >= 128 characters is excluded."
  (let ((kill-ring nil)
        (long-text (make-string 130 ?r)))
    (with-temp-buffer
      (insert long-text)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 1))
                ((symbol-function 'region-end) (lambda () (point-max))))
        (should-not (map-contains-key (annotated-completing-read--context-candidates) long-text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--context-candidates: current line

(ert-deftest annotated-completing-read/candidates-line-included ()
  "The current line is included as a candidate."
  (let ((kill-ring nil))
    (with-temp-buffer
      (insert "current line text")
      (goto-char (point-min))
      (should (map-contains-key (annotated-completing-read--context-candidates) "current line text")))))

(ert-deftest annotated-completing-read/candidates-line-annotation-format ()
  "Line annotation contains both 'line' and the buffer name."
  (let ((kill-ring nil))
    (with-temp-buffer
      (rename-buffer "line-test-buf" t)
      (insert "my line")
      (goto-char (point-min))
      (let ((annotation (map-elt (annotated-completing-read--context-candidates) "my line")))
        (should (string-match-p "line" annotation))
        (should (string-match-p "line-test-buf" annotation))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--context-candidates: thing at point

(ert-deftest annotated-completing-read/candidates-thing-at-point-in-prog-mode ()
  "Symbols at point in prog-mode buffers are included."
  (let ((kill-ring nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "my-symbol")
      (goto-char 5)
      (should (map-contains-key (annotated-completing-read--context-candidates) "my-symbol")))))

(ert-deftest annotated-completing-read/candidates-thing-at-point-annotation ()
  "thing-at-point candidates carry an 'at point' annotation."
  (let ((kill-ring nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      ;; Surround the word so the line candidate differs from the symbol.
      (insert "prefix my-func suffix")
      (goto-char 12)  ; inside "my-func"
      (let ((annotation (map-elt (annotated-completing-read--context-candidates) "my-func")))
        (should (stringp annotation))
        (should (string-match-p "at point" annotation))))))

(ert-deftest annotated-completing-read/candidates-thing-not-added-in-fundamental-mode ()
  "fundamental-mode produces no 'at point' candidates."
  (let ((kill-ring nil))
    (with-temp-buffer
      (insert "someword")
      (goto-char 4)
      (let* ((tbl (annotated-completing-read--context-candidates))
             (annotation (map-elt tbl "someword")))
        (should-not (and annotation (string-match-p "at point" annotation)))))))

(ert-deftest annotated-completing-read/candidates-thing-too-long-excluded ()
  "thing-at-point values with length >= 64 are excluded from at-point candidates."
  (let ((kill-ring nil)
        (long-word (make-string 65 ?w)))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert long-word)
      (goto-char 1)
      (let ((annotation (map-elt (annotated-completing-read--context-candidates) long-word)))
        ;; may appear as a line candidate, but must not be annotated as at-point
        (should-not (and annotation (string-match-p "at point" annotation)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read-context-from-point

(ert-deftest annotated-completing-read/context-from-point-returns-string ()
  (let ((kill-ring (list "candidate")))
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest _) "candidate")))
        (should (stringp (annotated-completing-read-context-from-point)))))))

(ert-deftest annotated-completing-read/context-from-point-returns-selection ()
  (let ((kill-ring (list "chosen")))
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest _) "chosen")))
        (should (equal "chosen" (annotated-completing-read-context-from-point)))))))

(ert-deftest annotated-completing-read/context-from-point-passes-prompt ()
  "The PROMPT argument is forwarded to annotated-completing-read."
  (let ((kill-ring (list "item"))
        received-prompt)
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest args)
                   (setq received-prompt (plist-get args :prompt))
                   "item")))
        (annotated-completing-read-context-from-point :prompt "my prompt: ")
        (should (equal "my prompt: " received-prompt))))))

(ert-deftest annotated-completing-read/context-from-point-history-defaults-to-this-command ()
  "When :history is unspecified, this-command is used as the history key."
  (let ((kill-ring (list "item"))
        received-history)
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest args)
                   (setq received-history (plist-get args :history))
                   "item")))
        (let ((this-command 'my-calling-command))
          (annotated-completing-read-context-from-point))
        (should (eq 'my-calling-command received-history))))))

(ert-deftest annotated-completing-read/context-from-point-explicit-history-key ()
  "An explicit :history symbol is forwarded to annotated-completing-read."
  (let ((kill-ring (list "item"))
        received-history)
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest args)
                   (setq received-history (plist-get args :history))
                   "item")))
        (annotated-completing-read-context-from-point
	 :prompt nil
	 :seed nil
	 :history 'my-history)
        (should (eq 'my-history received-history))))))

(ert-deftest annotated-completing-read/context-from-point-seed-in-candidates ()
  "The SEED argument produces candidates annotated as 'seed'."
  (let ((kill-ring nil)
        received-table)
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (tbl &rest _)
                   (setq received-table tbl)
                   "myseed")))
        (annotated-completing-read-context-from-point
	 :prompt nil
	 :seed "myseed")
        (should (map-contains-key received-table "myseed"))
        (should (equal "seed" (map-elt received-table "myseed")))))))

(ert-deftest annotated-completing-read/context-from-point-empty-returns-empty-string ()
  "Returns \"\" without prompting when no candidates are available."
  (let ((kill-ring nil))
    (with-temp-buffer
      ;; fundamental-mode, empty buffer, no kill ring, no region
      (should (equal "" (annotated-completing-read-context-from-point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--length-of-longest

(ert-deftest annotated-completing-read/length-of-longest-basic ()
  (should (= 5 (annotated-completing-read--length-of-longest '("ab" "hello" "hi")))))

(ert-deftest annotated-completing-read/length-of-longest-single-element ()
  (should (= 3 (annotated-completing-read--length-of-longest '("foo")))))

(ert-deftest annotated-completing-read/length-of-longest-all-same-length ()
  (should (= 3 (annotated-completing-read--length-of-longest '("foo" "bar" "baz")))))

(ert-deftest annotated-completing-read/length-of-longest-empty-string ()
  (should (= 0 (annotated-completing-read--length-of-longest '("")))))

(ert-deftest annotated-completing-read/length-of-longest-empty-list ()
  (should (= 0 (annotated-completing-read--length-of-longest '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--prefix-padding
;; Formula: (abs (+ 4 (- longest (length key))))

(ert-deftest annotated-completing-read/prefix-padding-key-shorter-than-longest ()
  ;; key="foo" len=3, longest=10 → abs(4 + (10-3)) = 11 spaces
  (let ((pad (annotated-completing-read--prefix-padding "foo" 10)))
    (should (stringp pad))
    (should (= 11 (length pad)))
    (should (string-match-p "^ +$" pad))))

(ert-deftest annotated-completing-read/prefix-padding-key-equals-longest ()
  ;; key="foo" len=3, longest=3 → abs(4 + 0) = 4 spaces
  (should (= 4 (length (annotated-completing-read--prefix-padding "foo" 3)))))

(ert-deftest annotated-completing-read/prefix-padding-key-longer-than-longest ()
  ;; key="foobar" len=6, longest=3 → abs(4 + (3-6)) = abs(1) = 1 space
  (should (= 1 (length (annotated-completing-read--prefix-padding "foobar" 3)))))

(ert-deftest annotated-completing-read/prefix-padding-returns-spaces ()
  (should (equal "    " (annotated-completing-read--prefix-padding "foo" 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--directory-clean

(ert-deftest annotated-completing-read/directory-clean-removes-nil ()
  "Nil entries in the input are removed."
  (let ((result (annotated-completing-read--directory-clean (list "/tmp/" nil "/usr/"))))
    (should-not (member nil result))))

(ert-deftest annotated-completing-read/directory-clean-removes-empty ()
  "Empty-string entries are removed."
  (let ((result (annotated-completing-read--directory-clean (list "/tmp/" "" "/usr/"))))
    (should-not (member "" result))))

(ert-deftest annotated-completing-read/directory-clean-removes-whitespace-only ()
  "Whitespace-only entries are removed."
  (let ((result (annotated-completing-read--directory-clean (list "/tmp/" "   " "/usr/"))))
    (should (seq-every-p (lambda (d) (not (string-blank-p d))) result))))

(ert-deftest annotated-completing-read/directory-clean-keeps-valid ()
  "Valid absolute paths survive cleaning."
  (let ((result (annotated-completing-read--directory-clean (list "/tmp/" "/usr/"))))
    (should (member "/tmp/" result))
    (should (member "/usr/" result))))

(ert-deftest annotated-completing-read/directory-clean-empty-input ()
  "Empty input list returns nil."
  (should (null (annotated-completing-read--directory-clean nil))))

(ert-deftest annotated-completing-read/directory-clean-all-nil ()
  "All-nil input returns nil."
  (should (null (annotated-completing-read--directory-clean (list nil nil nil)))))

(ert-deftest annotated-completing-read/directory-clean-deduplicates ()
  "Duplicate paths are removed."
  (let ((result (annotated-completing-read--directory-clean (list "/tmp/" "/tmp/" "/usr/"))))
    (should (= (length result) (length (seq-uniq result))))))

(ert-deftest annotated-completing-read/directory-clean-expands-relative ()
  "Relative paths are expanded to absolute paths."
  (let* ((default-directory "/tmp/")
         (result (annotated-completing-read--directory-clean (list "subdir"))))
    (should (cl-every #'file-name-absolute-p result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--directory-parents

(defmacro acr-directory-test--with-temp-tree (root-var dirs &rest body)
  "Bind ROOT-VAR to a temp directory, create DIRS under it, run BODY, clean up."
  (declare (indent 2))
  `(let ((,root-var (file-name-as-directory (make-temp-file "acr-dir-test" t))))
     (unwind-protect
         (progn
           (dolist (d (list ,@dirs))
             (make-directory (expand-file-name d ,root-var) t))
           ,@body)
       (delete-directory ,root-var t))))

(ert-deftest annotated-completing-read/directory-parents-includes-start ()
  "The starting directory appears in the output."
  (acr-directory-test--with-temp-tree root ("a/b/c")
    (let* ((start (file-name-as-directory (expand-file-name "a/b/c" root)))
           (result (annotated-completing-read--directory-parents start root)))
      (should (seq-some (lambda (d) (file-equal-p d start)) result)))))

(ert-deftest annotated-completing-read/directory-parents-includes-stop ()
  "The stop directory appears in the output."
  (acr-directory-test--with-temp-tree root ("a/b")
    (let* ((start (file-name-as-directory (expand-file-name "a/b" root)))
           (result (annotated-completing-read--directory-parents start root)))
      (should (seq-some (lambda (d) (file-equal-p d root)) result)))))

(ert-deftest annotated-completing-read/directory-parents-returns-list ()
  "The function returns a list."
  (let ((result (annotated-completing-read--directory-parents (expand-file-name "~/") (expand-file-name "~/"))))
    (should (listp result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read-directory annotation labels

(ert-deftest annotated-completing-read/directory-labels-current ()
  "The current directory is labelled 'current directory'."
  (let* ((dir (expand-file-name "/tmp/"))
         (default-directory dir))
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () dir))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (should (equal "current directory (project root)" (map-elt tbl dir)))
                 dir)))
      (annotated-completing-read-directory :candidates (list dir)))))

(ert-deftest annotated-completing-read/directory-labels-project-root ()
  "A directory matching the project root is labelled 'project root'."
  (let* ((root (expand-file-name "/tmp/project/"))
         (other (expand-file-name "/tmp/other/"))
         (default-directory other))
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () root))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (should (equal "project root" (map-elt tbl root)))
                 root)))
      (annotated-completing-read-directory :candidates (list root)))))

(ert-deftest annotated-completing-read/directory-labels-parent ()
  "A directory that is an ancestor of the current dir is labelled 'parent'."
  (let* ((parent (expand-file-name "/tmp/"))
         (child (expand-file-name "/tmp/sub/"))
         (default-directory child))
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () parent))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (should (member (map-elt tbl parent) '("project root" "parent")))
                 parent)))
      (annotated-completing-read-directory :candidates (list parent)))))

(ert-deftest annotated-completing-read/directory-prompt-forwarded ()
  "The :prompt keyword is forwarded to annotated-completing-read."
  (let* ((dir (expand-file-name "/tmp/"))
         (default-directory dir)
         received-prompt)
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () dir))
              ((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest args)
                 (setq received-prompt (plist-get args :prompt))
                 dir)))
      (annotated-completing-read-directory :candidates (list dir) :prompt "pick: ")
      (should (equal "pick: " received-prompt)))))

(ert-deftest annotated-completing-read/directory-candidates-override ()
  "When :candidates is provided, default candidates are not computed."
  (let* ((dir (expand-file-name "/tmp/"))
         (default-directory dir)
         received-table)
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () dir))
              ((symbol-function 'annotated-completing-read--directory-default-candidates)
               (lambda () (error "should not be called")))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (setq received-table tbl)
                 dir)))
      (annotated-completing-read-directory :candidates (list dir))
      (should (map-contains-key received-table dir)))))

(ert-deftest annotated-completing-read/directory-no-groups-below-threshold ()
  "No :group-name is passed when there are 8 or fewer candidates."
  (let* ((dirs (mapcar (lambda (n) (format "/tmp/dir%d/" n)) (number-sequence 1 8)))
         (default-directory "/tmp/dir1/")
         received-group-name)
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () "/tmp/dir1/"))
              ((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest args)
                 (setq received-group-name (plist-get args :group-name))
                 "/tmp/dir1/")))
      (annotated-completing-read-directory :candidates dirs)
      (should (null received-group-name)))))

(ert-deftest annotated-completing-read/directory-groups-above-threshold ()
  ":group-name is a function when there are more than 8 candidates."
  (let* ((dirs (mapcar (lambda (n) (format "/tmp/dir%d/" n)) (number-sequence 1 9)))
         (default-directory "/tmp/dir1/")
         received-group-name)
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () "/tmp/dir1/"))
              ((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest args)
                 (setq received-group-name (plist-get args :group-name))
                 "/tmp/dir1/")))
      (annotated-completing-read-directory :candidates dirs)
      (should (functionp received-group-name)))))

(ert-deftest annotated-completing-read/directory-group-labels ()
  "The group function returns the relationship label directly."
  (let* ((root "/tmp/project/")
         (current "/tmp/project/src/")
         (parent "/tmp/")
         (child "/tmp/project/src/sub/")
         (sibling "/tmp/project/lib/")
         (other "/home/user/")
         (dirs (list root current parent child sibling other
                     "/a/" "/b/" "/c/"))  ; pad to >8
         (default-directory current)
         received-group-fn)
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () root))
              ((symbol-function 'annotated-completing-read--directory-entry-counts) (lambda (_) ""))
              ((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest args)
                 (setq received-group-fn (plist-get args :group-name))
                 current)))
      (annotated-completing-read-directory :candidates dirs)
      (should (functionp received-group-fn))
      (should (equal "current directory" (funcall received-group-fn current)))
      (should (equal "project root"      (funcall received-group-fn root)))
      (should (equal "child"             (funcall received-group-fn child)))
      (should (equal "parent"            (funcall received-group-fn parent)))
      (should (equal "other"             (funcall received-group-fn other))))))

(ert-deftest annotated-completing-read/directory-grouped-annotation-is-counts ()
  "When grouped, the table passed to annotated-completing-read contains entry counts."
  (let* ((dirs (mapcar (lambda (n) (format "/tmp/dir%d/" n)) (number-sequence 1 9)))
         (default-directory "/tmp/dir1/")
         received-table)
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () "/tmp/dir1/"))
              ((symbol-function 'annotated-completing-read--directory-entry-counts)
               (lambda (d) (format "counts:%s" d)))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (setq received-table tbl)
                 "/tmp/dir1/")))
      (annotated-completing-read-directory :candidates dirs)
      (should (hash-table-p received-table))
      (should (string-prefix-p "counts:" (map-elt received-table "/tmp/dir1/"))))))

(ert-deftest annotated-completing-read/directory-ungrouped-annotation-is-relationship ()
  "When not grouped (<=8 items), the table contains relationship labels."
  (let* ((root "/tmp/project/")
         (current "/tmp/project/src/")
         (dirs (list root current "/a/" "/b/"))
         (default-directory current)
         received-table)
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () root))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (setq received-table tbl)
                 current)))
      (annotated-completing-read-directory :candidates dirs)
      (should (equal "project root"      (map-elt received-table root)))
      (should (equal "current directory" (map-elt received-table current))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--filter-directories

(ert-deftest annotated-completing-read/filter-directories-removes-non-strings ()
  "Non-string entries (nil, numbers) are dropped."
  (let ((result (annotated-completing-read--filter-directories (list "/tmp/" nil 42 "/tmp/"))))
    (should (cl-every #'stringp result))))

(ert-deftest annotated-completing-read/filter-directories-regular-file-becomes-directory ()
  "A regular file path is replaced by its parent directory."
  (let* ((f (make-temp-file "acr-test"))
         (expected (file-name-as-directory (file-name-directory f)))
         (result (annotated-completing-read--filter-directories (list f))))
    (unwind-protect
        (should (member expected result))
      (delete-file f))))

(ert-deftest annotated-completing-read/filter-directories-nonexistent-excluded ()
  "Paths that are not existing directories are excluded."
  (let ((result (annotated-completing-read--filter-directories
                 (list "/tmp/this-does-not-exist-acr-test/"))))
    (should (null result))))

(ert-deftest annotated-completing-read/filter-directories-deduplicates ()
  "Duplicate directory paths are collapsed to one entry."
  (let ((result (annotated-completing-read--filter-directories (list "/tmp/" "/tmp/" "/tmp/"))))
    (should (= 1 (length result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--directory-entry-counts

(ert-deftest annotated-completing-read/directory-entry-counts-format ()
  "Returns 'N dirs, M files' for an accessible directory."
  (acr-directory-test--with-temp-tree root ("sub1" "sub2")
    (write-region "" nil (expand-file-name "file.txt" root))
    (let ((result (annotated-completing-read--directory-entry-counts root)))
      (should (string-match-p "[0-9]+ dirs" result))
      (should (string-match-p "[0-9]+ files" result)))))

(ert-deftest annotated-completing-read/directory-entry-counts-inaccessible ()
  "Returns \"\" for a path that is not an accessible directory."
  (should (equal "" (annotated-completing-read--directory-entry-counts "/no/such/path/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--directory-buffer-count

(ert-deftest annotated-completing-read/directory-buffer-count-finds-buffer-under-dir ()
  "Counts a live buffer whose file lives under the given directory."
  (acr-directory-test--with-temp-tree root ("sub1")
    (let ((buf (find-file-noselect (expand-file-name "sub1/file.txt" root))))
      (unwind-protect
          (should (= 1 (annotated-completing-read--directory-buffer-count root)))
        (kill-buffer buf)))))

(ert-deftest annotated-completing-read/directory-buffer-count-excludes-sibling ()
  "Does not count a buffer in a sibling directory with a shared name prefix."
  (acr-directory-test--with-temp-tree root ("sub1")
    (let* ((sibling (concat (directory-file-name root) "-sibling"))
           (buf (progn (make-directory sibling t)
                       (find-file-noselect (expand-file-name "file.txt" sibling)))))
      (unwind-protect
          (progn
            (should (= 0 (annotated-completing-read--directory-buffer-count root)))
            (should (= 1 (annotated-completing-read--directory-buffer-count sibling))))
        (kill-buffer buf)
        (delete-directory sibling t)))))

(ert-deftest annotated-completing-read/directory-buffer-count-zero-for-no-buffers ()
  "Returns 0 for a directory with no visiting buffers."
  (acr-directory-test--with-temp-tree root ("sub1")
    (should (= 0 (annotated-completing-read--directory-buffer-count root)))))

(ert-deftest annotated-completing-read/directory-entry-counts-includes-buffer-count ()
  "Appends the buffer count to the dirs/files annotation when buffers are open."
  (acr-directory-test--with-temp-tree root ("sub1")
    (let ((buf (find-file-noselect (expand-file-name "sub1/file.txt" root))))
      (unwind-protect
          (should (string-match-p "1 buffer\\b"
                                   (annotated-completing-read--directory-entry-counts root)))
        (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--ensure-history

(ert-deftest annotated-completing-read/ensure-history-noop-when-valid ()
  "Does nothing when history is already a hash table."
  (let ((annotated-completing-read-history (make-hash-table :test #'equal)))
    (map-put! annotated-completing-read-history 'cmd '("a"))
    (annotated-completing-read--ensure-history)
    (should (equal '("a") (map-elt annotated-completing-read-history 'cmd)))))

(ert-deftest annotated-completing-read/ensure-history-resets-corrupt-value ()
  "Resets history to a fresh hash table when the stored value is not a hash table."
  (let ((annotated-completing-read-history "corrupt"))
    (annotated-completing-read--ensure-history)
    (should (hash-table-p annotated-completing-read-history))))

(ert-deftest annotated-completing-read/ensure-history-converts-alist ()
  "Converts an alist (as savehist may restore) into a hash table preserving entries."
  (let ((annotated-completing-read-history '((cmd1 . ("a" "b")) (cmd2 . ("c")))))
    (annotated-completing-read--ensure-history)
    (should (hash-table-p annotated-completing-read-history))
    (should (equal '("a" "b") (map-elt annotated-completing-read-history 'cmd1)))
    (should (equal '("c") (map-elt annotated-completing-read-history 'cmd2)))))

(ert-deftest annotated-completing-read/ensure-history-converts-empty-alist ()
  "An empty list is promoted to an empty hash table."
  (let ((annotated-completing-read-history '()))
    (annotated-completing-read--ensure-history)
    (should (hash-table-p annotated-completing-read-history))
    (should (= 0 (hash-table-count annotated-completing-read-history)))))

(ert-deftest annotated-completing-read/clear-history-resets-to-empty ()
  "clear-history replaces the history table with an empty hash table."
  (let ((annotated-completing-read-history (make-hash-table :test #'equal)))
    (map-put! annotated-completing-read-history 'some-cmd '("a" "b"))
    (annotated-completing-read-clear-history)
    (should (hash-table-p annotated-completing-read-history))
    (should (= 0 (hash-table-count annotated-completing-read-history)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read-context-from-point — initial-input

(ert-deftest annotated-completing-read/context-from-point-initial-input-forwarded ()
  "The :initial-input argument is forwarded to annotated-completing-read."
  (let ((kill-ring (list "item"))
        received-initial)
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest args)
                   (setq received-initial (plist-get args :initial-input))
                   "item")))
        (annotated-completing-read-context-from-point :initial-input "pre")
        (should (equal "pre" received-initial))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read-directory — additional relationship labels

(ert-deftest annotated-completing-read/directory-labels-child ()
  "A subdirectory of the current dir is labelled 'child'."
  (let* ((current (expand-file-name "/tmp/"))
         (child   (expand-file-name "/tmp/sub/"))
         (default-directory current))
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () current))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (should (equal "child" (map-elt tbl child)))
                 current)))
      (annotated-completing-read-directory :candidates (list child)))))

(ert-deftest annotated-completing-read/directory-labels-sibling ()
  "A directory sharing the parent of current dir is labelled 'sibling'."
  (let* ((current (expand-file-name "/tmp/a/"))
         (sibling (expand-file-name "/tmp/b/"))
         (default-directory current))
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () current))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (should (equal "sibling" (map-elt tbl sibling)))
                 current)))
      (annotated-completing-read-directory :candidates (list sibling)))))

(ert-deftest annotated-completing-read/directory-require-match-forwarded ()
  "The :require-match keyword is forwarded to annotated-completing-read."
  (let* ((dir (expand-file-name "/tmp/"))
         (default-directory dir)
         received-require-match)
    (cl-letf (((symbol-function 'annotated-completing-read--project-root) (lambda () dir))
              ((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest args)
                 (setq received-require-match (plist-get args :require-match))
                 dir)))
      (annotated-completing-read-directory :candidates (list dir) :require-match t)
      (should (eq t received-require-match)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read — empty table

(ert-deftest annotated-completing-read/empty-table ()
  "Accepts an empty hash table and returns whatever completing-read returns."
  (let ((table (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll &rest _) "")))
      (should (equal "" (annotated-completing-read table))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--directory-entry-counts — empty directory

(ert-deftest annotated-completing-read/directory-entry-counts-empty-dir ()
  "Returns '0 dirs, 0 files' for an empty accessible directory."
  (acr-directory-test--with-temp-tree root ()
    (let ((result (annotated-completing-read--directory-entry-counts root)))
      (should (string-match-p "0 dirs" result))
      (should (string-match-p "0 files" result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--directory-parents — start equals stop

(ert-deftest annotated-completing-read/directory-parents-start-equals-stop ()
  "When start and stop are the same path the while loop does not execute."
  (acr-directory-test--with-temp-tree root ()
    (let ((result (annotated-completing-read--directory-parents root root)))
      (should (= 1 (length result)))
      (should (seq-some (lambda (d) (file-equal-p d root)) result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; savehist / desktop integration (top-level registration)

(ert-deftest annotated-completing-read/savehist-additional-variables ()
  "Loading ACR registers history in savehist-additional-variables."
  (should (member 'annotated-completing-read-history savehist-additional-variables)))

(ert-deftest annotated-completing-read/savehist-mode-hook ()
  "Loading ACR registers ensure-history on savehist-mode-hook."
  (should (memq #'annotated-completing-read--ensure-history savehist-mode-hook)))

(ert-deftest annotated-completing-read/desktop-globals-to-save ()
  "Loading ACR registers history in desktop-globals-to-save."
  (require 'desktop)
  (should (member 'annotated-completing-read-history desktop-globals-to-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read — :default keyword

(ert-deftest annotated-completing-read/default-returned-for-empty-table ()
  "Returns :default immediately when table is empty, without calling completing-read."
  (let ((table (make-hash-table :test #'equal))
        called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (setq called t) "")))
      (should (equal "fallback" (annotated-completing-read table :default "fallback")))
      (should-not called))))

(ert-deftest annotated-completing-read/default-returned-on-empty-input ()
  "Returns :default when completing-read returns empty string."
  (let ((table (acr-test--ht ("a" "ann"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "")))
      (should (equal "fallback" (annotated-completing-read table :default "fallback"))))))

(ert-deftest annotated-completing-read/default-returned-on-quit ()
  "Returns :default when the user quits with C-g."
  (let ((table (acr-test--ht ("a" "ann"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil))))
      (should (equal "fallback" (annotated-completing-read table :default "fallback"))))))

(ert-deftest annotated-completing-read/default-passed-to-completing-read ()
  "The :default value is forwarded as completing-read's DEF argument."
  (let ((table (acr-test--ht ("a" "ann")))
        received-def)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _req _init _hist def &rest _)
                 (setq received-def def)
                 "a")))
      (annotated-completing-read table :default "fallback")
      (should (equal "fallback" received-def)))))

(ert-deftest annotated-completing-read/default-nil-propagates-quit ()
  "When :default is nil (the default), C-g propagates as a quit signal."
  (let ((table (acr-test--ht ("a" "ann")))
        quit-caught)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil))))
      (condition-case nil
          (annotated-completing-read table)
        (quit (setq quit-caught t))))
    (should quit-caught)))

(ert-deftest annotated-completing-read/default-not-returned-on-selection ()
  "Normal selection is returned unchanged even when :default is set."
  (let ((table (acr-test--ht ("a" "ann"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "a")))
      (should (equal "a" (annotated-completing-read table :default "fallback"))))))

(ert-deftest annotated-completing-read/or-nil-returns-nil-on-quit ()
  "With :or-nil t, quitting returns nil instead of propagating."
  (let ((table (acr-test--ht ("a" "ann"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil))))
      (should-not (annotated-completing-read table :or-nil t)))))

(ert-deftest annotated-completing-read/or-nil-returns-nil-on-empty-input ()
  "With :or-nil t, empty input returns nil."
  (let ((table (acr-test--ht ("a" "ann"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "")))
      (should-not (annotated-completing-read table :or-nil t)))))

(ert-deftest annotated-completing-read/or-nil-empty-table-returns-nil ()
  "With :or-nil t, an empty table returns nil without prompting."
  (let ((table (make-hash-table :test #'equal))
        called)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (setq called t) "")))
      (should-not (annotated-completing-read table :or-nil t))
      (should-not called))))

(ert-deftest annotated-completing-read/default-takes-precedence-over-or-nil ()
  "When both :default and :or-nil are set, :default value is returned."
  (let ((table (acr-test--ht ("a" "ann"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil))))
      (should (equal "fallback" (annotated-completing-read table :default "fallback" :or-nil t))))))

(ert-deftest annotated-completing-read/or-nil-selection-returned ()
  "With :or-nil t, a real selection is still returned unchanged."
  (let ((table (acr-test--ht ("a" "ann"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "a")))
      (should (equal "a" (annotated-completing-read table :or-nil t))))))

;;; --to-map unit tests

(ert-deftest annotated-completing-read/to-map-hash-passthrough ()
  "Hash tables are returned as-is (identity, no copy)."
  (let* ((ht (acr-test--ht ("a" "1")))
         (result (annotated-completing-read--to-map ht)))
    (should (eq ht result))))

(ert-deftest annotated-completing-read/to-map-dotted-alist ()
  (let ((result (annotated-completing-read--to-map '(("a" . "1") ("b" . "2")))))
    (should (hash-table-p result))
    (should (equal "1" (map-elt result "a")))
    (should (equal "2" (map-elt result "b")))))

(ert-deftest annotated-completing-read/to-map-list-form-alist ()
  (let ((result (annotated-completing-read--to-map '(("a" "1") ("b" "2")))))
    (should (hash-table-p result))
    (should (equal "1" (map-elt result "a")))
    (should (equal "2" (map-elt result "b")))))

(ert-deftest annotated-completing-read/to-map-nil-annotation ()
  (let ((result (annotated-completing-read--to-map '(("a" . nil) ("b" . "2")))))
    (should (hash-table-p result))
    (should (null (map-elt result "a")))
    (should (equal "2" (map-elt result "b")))))

(ert-deftest annotated-completing-read/to-map-empty-alist ()
  (let ((result (annotated-completing-read--to-map '())))
    (should (hash-table-p result))
    (should (= 0 (hash-table-count result)))))

(ert-deftest annotated-completing-read/to-map-rejects-string ()
  (should-error (annotated-completing-read--to-map "not a table") :type 'user-error))

(ert-deftest annotated-completing-read/to-map-rejects-vector ()
  (should-error (annotated-completing-read--to-map [a b c]) :type 'user-error))

;;; Alist input acceptance

(ert-deftest annotated-completing-read/accepts-dotted-alist ()
  (let ((table '(("foo" . "desc-foo") ("bar" . "desc-bar"))))
    (acr-with-mock table "foo"
      (should (equal "foo" (annotated-completing-read table))))))

(ert-deftest annotated-completing-read/accepts-list-form-alist ()
  (let ((table '(("foo" "desc-foo") ("bar" "desc-bar"))))
    (acr-with-mock table "foo"
      (should (equal "foo" (annotated-completing-read table))))))

(ert-deftest annotated-completing-read/accepts-empty-alist ()
  (acr-with-mock '() ""
    (should (equal "" (annotated-completing-read '())))))

(ert-deftest annotated-completing-read/dotted-alist-annotation-content ()
  (let ((table '(("alpha" . "first letter") ("beta" . "second letter"))))
    (acr-with-mock table "alpha"
      (annotated-completing-read table)
      (let ((annotate (alist-get 'annotation-function (acr-metadata captured-collection))))
        (should (string-match-p "first letter"  (funcall annotate "alpha")))
        (should (string-match-p "second letter" (funcall annotate "beta")))))))

(ert-deftest annotated-completing-read/list-form-alist-annotation-content ()
  (let ((table '(("alpha" "first letter") ("beta" "second letter"))))
    (acr-with-mock table "alpha"
      (annotated-completing-read table)
      (let ((annotate (alist-get 'annotation-function (acr-metadata captured-collection))))
        (should (string-match-p "first letter"  (funcall annotate "alpha")))
        (should (string-match-p "second letter" (funcall annotate "beta")))))))

(ert-deftest annotated-completing-read/nil-annotation-entry ()
  "A nil annotation returns nil from the annotation function."
  (let ((table '(("foo" . nil) ("bar" . "has-ann"))))
    (acr-with-mock table "foo"
      (annotated-completing-read table)
      (let ((annotate (alist-get 'annotation-function (acr-metadata captured-collection))))
        (should (null (funcall annotate "foo")))
        (should (string-match-p "has-ann" (funcall annotate "bar")))))))

(ert-deftest annotated-completing-read/dotted-alist-collection-returns-candidates ()
  "All alist keys are surfaced as completion candidates."
  (let ((table '(("alpha" . "1") ("beta" . "2") ("gamma" . "3"))))
    (acr-with-mock table "alpha"
      (annotated-completing-read table)
      (let ((all (funcall captured-collection "" nil t)))
        (should (member "alpha" all))
        (should (member "beta"  all))
        (should (member "gamma" all))))))

(ert-deftest annotated-completing-read/dotted-alist-annotation-alignment ()
  "Column alignment holds for dotted alist input."
  (let ((table '(("a" . "x") ("much-longer-key" . "y"))))
    (acr-with-mock table "a"
      (annotated-completing-read table)
      (let* ((annotate  (alist-get 'annotation-function (acr-metadata captured-collection)))
             (ann-short (funcall annotate "a"))
             (ann-long  (funcall annotate "much-longer-key")))
        (should (= (+ (length "a")               (length ann-short))
                   (+ (length "much-longer-key") (length ann-long))))))))

(provide 'test-annotated-completing-read)
;;; test-annotated-completing-read.el ends here
