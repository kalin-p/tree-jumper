;;; tree-jumper.el --- Hint interface for navigating tree-sitter syntax trees -*- lexical-binding: t; -*-

(require 'treesit)
(require 'color)

(defvar-local tree-jumper-current-node nil)

;; varibles named like tree-jumper-*-hints hold lists of characters to be used
;; as hints. They are easily composed by adding the names of the ones you want
;; to use to ~tree-jumper-use-hints~.
(defvar tree-jumper-home-row-hints
  '("a" "s" "d" "f" "g" "h" "j" "k" "l"))

(defvar tree-jumper-upper-row-hints
  '("q" "w" "e" "r" "t" "y" "u" "i" "o" "p"))

(defvar tree-jumper-lower-row-hints
  '("z" "x" "c" "v" "b" "n" "m"))

(defvar tree-jumper-number-row-hints
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))

(defvar tree-jumper-use-hints '(tree-jumper-home-row-hints tree-jumper-upper-row-hints tree-jumper-lower-row-hints))

(defvar tree-jumper-max-hints 300)

(defvar tree-jumper-hint-alphabet ()
  "List of letters that will be used to construct hints")

(defun tree-jumper-filter-out-command-keys (hint-space keymap)
  "Function that removes any characters that have been bound as keys
  in the command keymap from the list of characters that will be
  used as hints."
  (if (keymapp keymap)
      (let ((hint hint-space)
            (keep ()))
        (while hint
          (unless (assoc (string-to-char (car hint)) keymap)
            (push (car hint) keep))
          (setq hint (cdr hint)))
        (reverse keep))
    (error "tree-jumper-filter-out-command-keys requires a keymap as its second argument.")))

;; Generating hints is the same as enumerating objects. The difference is that
;; we want the generated numbers to be written in a way that is easy and quick
;; to type on a keyboard. So instead of using normal base 10 it is more fitting
;; to use base 26 for example and use the English alphabet as numerals.
(defun convert-base10-to-alphabet-radix (num)
  "Write a base-10 number as a string with the numerals in
tree-jumper-hint-alphabet."
  (let ((indices ())
        (radix (length tree-jumper-hint-alphabet)))
    (while (>= num radix)
      (push (/ num radix) indices)
      (setq num (mod num radix)))
    (push num indices)
    (apply #'concat
           (mapcar (lambda (i)
                     (nth i tree-jumper-hint-alphabet))
                   (reverse indices)))))

(defun convert-alphabet-radix-to-base10 (str)
  "Convert a number written as a string of tree-jumper-hint-alphabet
numerals to an integer."
  (let ((num 0)
        (l (1- (length str))))
    (mapc (lambda (char)
            (setq num
                  (+ num
                     (* (cl-position (char-to-string char)
                                     tree-jumper-hint-alphabet
                                     :test #'equal)
                        (expt (length tree-jumper-hint-alphabet) l))))
            (setq l (1- l)))
          str)
    num))

(defvar tree-jumper-hint-keymaps []
  "Holds a keymap for each character in a hint, e.g. if we have
hints that are 3 characters long it will have 3 keymaps. All
keymaps except the last one are prefix keymaps and point ot the
next one. The final keymap uses a function that tranlates the
accumulated command (via this-command-keys) to a tree-sitter node
position")

(defun tree-jumper-get-keymap (level)
  (aref tree-jumper-hint-keymaps (1- level)))

(defun tree-jumper-make-keymap (level)
  (cond ((< level 0)
         (error "keymap level has to be at least 0"))
        ((= level 0)
         (let ((keymap (define-keymap))
               (cmd #'tree-jumper-jump))
           (mapc (lambda (hint)
                   (define-key keymap hint cmd))
                   tree-jumper-hint-alphabet)
           keymap))
        (t
         (let ((keymap (define-keymap)) ;
               (cmd (aref tree-jumper-hint-keymaps
                          (1- level))))
           (mapcar (lambda (hint)
                     (define-key keymap hint cmd))
                   tree-jumper-hint-alphabet)
           keymap))))

(defun tree-jumper-ensure-hint-keymaps (hint-width)
  (let* ((n-keymaps (length tree-jumper-hint-keymaps)))
    (while (< n-keymaps hint-width)
      (setq tree-jumper-hint-keymaps
            (vconcat tree-jumper-hint-keymaps
                     (list (tree-jumper-make-keymap n-keymaps))))
      (setq n-keymaps (1+ n-keymaps)))))

(defvar-local tree-jumper-hint-vec [])

(defun tree-jumper-jump ()
  "Tranlates hint to a tree-sitter node and sets the point there."
  (interactive)
  (let ((dest (aref tree-jumper-hint-vec
                    (convert-alphabet-radix-to-base10 (this-command-keys)))))
    (when dest
      (goto-char (treesit-node-start dest))
      (tree-jumper-update (treesit-node-parent dest)))))

(defun tree-jumper-sparse-tree-pred (range-start range-end ts-node)
  "This predicate is used by tree-jumper to determine which syntax
tree nodes to hint"
  (and (> (treesit-node-start ts-node) range-start)
       (< (treesit-node-end ts-node) range-end)
       (treesit-node-field-name ts-node)
       (= 0 (length (treesit-node-children ts-node t)))))


(defvar-local tree-jumper-node-count 0)
(defvar-local tree-jumper-depth 30)

(defun tree-jumper-bfs (node)
  "This function does a breadth-first search on the nodes in the
syntax tree and appends them to a list."
  (let* ((depth 0)
         (range-start (window-start))
         (range-end (window-end nil t))
         (tree (treesit-induce-sparse-tree
                node
                (apply-partially #'tree-jumper-sparse-tree-pred range-start range-end)
                nil
                tree-jumper-depth))
         (count 0))
    (while tree
      (setq tree
            (apply #'append
                   (remove nil
                           (mapcar
                            (lambda (arg)
                              (cond ((listp arg) arg)
                                    ((treesit-node-p arg)
                                     (aset tree-jumper-hint-vec
                                           count
                                           arg)
                                     (setq count (1+ count))
                                     nil)))
                              tree)))))
    (setq tree-jumper-node-count count)))

(defvar-local tree-jumper-is-active nil
  "This var tracks weather the minor mode is in active or inactive
state.")

(defvar tree-jumper-active-keymap
  (define-keymap :full t)
  "Keymap used by tree-jumper in its active state. It is defined
with :full t, to discourage using other commands at the same time
as using hints.")

(defun tree-jumper-activate ()
  (interactive)
  (tree-jumper-update (treesit-buffer-root-node))
  (setq tree-jumper-is-active t)
  (setcdr (assoc 'tree-jumper minor-mode-map-alist) tree-jumper-active-keymap))

(defvar tree-jumper-inactive-keymap
  (define-keymap
    "C-j" #'tree-jumper-activate)
  "Keymap used when tree-jumper is in inactive state. It only needs
to hold the command to activate it.")

(defun tree-jumper-suspend ()
  (interactive)
  (remove-overlays)
  (setq tree-jumper-is-active nil)
  (setcdr (assoc 'tree-jumper minor-mode-map-alist) tree-jumper-inactive-keymap))

(defun tree-jumper-goto-root ()
  (interactive)
  (let* ((root (treesit-buffer-root-node))
         ;; (node (treesit-node-at (point)))
         (parent (treesit-node-parent tree-jumper-current-node)))
    (if (or (treesit-node-eq parent root)
            (treesit-node-eq tree-jumper-current-node root))
        (tree-jumper-update root)
      (tree-jumper-update (treesit-parent-until tree-jumper-current-node
                                                (lambda (arg)
                                                  (treesit-node-eq (treesit-node-parent arg) root)))))))

(defvar tree-jumper-command-keymap 
  (define-keymap
    "q" #'tree-jumper-suspend
    "u" #'tree-jumper-goto-root
    "C-v" #'scroll-up-command
    "M-v" #'scroll-down-command)
  "Keymap that defines commands to be used while hints are active.")

;; The following variables determine how hints are colored. Colors are generated
;; from the color of the background by adding random values factorized by these
;; coefficients.
(defvar tree-jumper-hue-rand-coef 0.3)
(defvar tree-jumper-saturation-rand-coef 0.0)
(defvar tree-jumper-luminance-rand-coef 0.0)

(defvar tree-jumper-hue-base 0.0)
(defvar tree-jumper-saturation-base 0.0)
(defvar tree-jumper-luminance-base 0.1)
(defvar tree-jumper-random-color-seed "")

(defun tree-jumper-wrap-float-0-1 (value)
  (cond ((and (>= value 0.0) (<= value 1.0))
         value)
        ((> value 1.0)
         (- value (truncate  value)))
        ((< value 0.0)
         (- 1 (abs (- value (truncate value)))))))

(defun tree-jumper-clamp-float-0-1 (value)
  (cond ((and (>= value 0.0) (<= value 1.0))
         value)
        ((> value 1.0)
         1.0)
        ((< value 0.0)
         0.0)))

(defvar tree-jumper-background-color-hsl
  (apply #'color-rgb-to-hsl
         (color-name-to-rgb (face-background 'default))))

(defun tree-jumper-hsl-to-hex (hsl)
  (apply #'color-rgb-to-hex
         (apply #'color-hsl-to-rgb hsl)))

(defun tree-jumper-generate-color (letter anchor-color)
  (let* ((rand-val (/ (float (random (concat letter
                                             tree-jumper-random-color-seed)))
                      (float most-positive-fixnum)))
	 (hue (tree-jumper-wrap-float-0-1
               (+ (car anchor-color)
		  tree-jumper-hue-base
		  (* rand-val tree-jumper-hue-rand-coef))))
	 (saturation (tree-jumper-clamp-float-0-1
		      (+ (cadr anchor-color)
			 tree-jumper-saturation-base
			 (* rand-val tree-jumper-saturation-rand-coef))))
	 (luminance (tree-jumper-clamp-float-0-1
		     (+ (caddr anchor-color)
			tree-jumper-luminance-base
			(* rand-val tree-jumper-luminance-rand-coef)))))
    '(hue saturation luminance)))

(defvar tree-jumper-hint-colors [])

(defvar tree-jumper-overlay-plist
  '(:hint-prefix-string " "
    :hint-suffix-string " "
    :hint-fg-lighten-percent 250
    :hint-fg-saturate-percent 40
    :hint-color-item nil))

;; To display hits overlays are used. This does not change the buffer in any
;; way, which would have been really annoying.
(defvar tree-jumper-overlays []
  "Vector that contains the overlays used to display hints to the user.")

(defun tree-jumper-hsl-saturate-lighten (hsl saturate-percent lighten-percent)
  (let ((saturated (apply #'color-saturate-hsl (append hsl (list saturate-percent)))))
    (apply #'color-lighten-hsl (append saturated (list lighten-percent)))))

(defun tree-jumper-make-overlay (num width)
  (let* ((hint (convert-base10-to-alphabet-radix num))
         (padded-hint (concat (make-string (- width (length hint)) ?a) hint))
         (node (aref tree-jumper-hint-vec num))
         (overlay (make-overlay
                  (treesit-node-start node)
                  (treesit-node-end node)))
         (color (aref tree-jumper-hint-colors num))
         (fg-color (tree-jumper-hsl-saturate-lighten
                    color
                    (plist-get tree-jumper-overlay-plist :hint-fg-saturate-percent)
                    (plist-get tree-jumper-overlay-plist :hint-fg-lighten-percent))))

    (overlay-put overlay
                 'before-string
                 (propertize (concat (plist-get
                                      tree-jumper-overlay-plist
                                      :hint-prefix-string)
                                     padded-hint
                                     (plist-get
                                      tree-jumper-overlay-plist
                                      :hint-suffix-string))
                             'face `(:foreground ,(tree-jumper-hsl-to-hex fg-color)
                                     :height 0.8
                                     :width 'expanded
                                     :background ,(tree-jumper-hsl-to-hex color))))

    (when (plist-get tree-jumper-overlay-plist :hint-color-item)
      (overlay-put overlay
                   'face `(:background ,(tree-jumper-hsl-to-hex color))))
    overlay))

(defun tree-jumper-remove-overlays ()
  (mapc (lambda (overlay) (when (overlayp overlay)
                            (delete-overlay overlay)))
        tree-jumper-overlays))

(defun tree-jumper-visualize-hints ()
  (let ((hint-width (length (convert-base10-to-alphabet-radix tree-jumper-node-count))))
    (dotimes (i tree-jumper-node-count)
      (aset tree-jumper-overlays i (tree-jumper-make-overlay i hint-width)))))

(defun tree-jumper-update (ts-node &optional named)
  (setq tree-jumper-current-node ts-node)
  (tree-jumper-remove-overlays)
  (tree-jumper-bfs ts-node)

  (let ((hint-width (length (convert-base10-to-alphabet-radix tree-jumper-node-count))))
    (tree-jumper-ensure-hint-keymaps hint-width)
    (setq tree-jumper-active-keymap
          (define-keymap
            :full t
            :parent (make-composed-keymap tree-jumper-command-keymap
                                          (tree-jumper-get-keymap hint-width))))
    (setcdr (assoc 'tree-jumper minor-mode-map-alist) tree-jumper-active-keymap))

  (tree-jumper-visualize-hints))

(defun tree-jumper-on-scroll (window new-start-pos)
  (when tree-jumper-is-active
    (tree-jumper-update (treesit-buffer-root-node))))

(defun tree-jumper-setup ()
  (let ((declared-hints (apply #'append (mapcar (lambda (sym)
                                                  (symbol-value sym))
                                                tree-jumper-use-hints))))
    (setq tree-jumper-hint-alphabet
          (tree-jumper-filter-out-command-keys declared-hints
                                               tree-jumper-command-keymap)))

  (setq tree-jumper-hint-vec (make-vector tree-jumper-max-hints nil)
        tree-jumper-hint-colors (make-vector tree-jumper-max-hints nil)
        tree-jumper-overlays (make-vector tree-jumper-max-hints nil))

  (setq tree-jumper-background-color-hsl
        (apply #'color-rgb-to-hsl
               (color-name-to-rgb (face-background 'default))))

  (dotimes (i tree-jumper-max-hints)
    (aset tree-jumper-hint-colors
          i
          (tree-jumper-generate-color
           (convert-base10-to-alphabet-radix i)
           tree-jumper-background-color-hsl)))

  (add-hook 'window-scroll-functions #'tree-jumper-on-scroll nil t)

  (setq tree-jumper-is-active t)
  (tree-jumper-update (treesit-buffer-root-node) nil))

(define-minor-mode tree-jumper
  "Some docs"

  :lighter " 🌳🐒"
  ;; :keymap tree-jumper-active-keymap
  :keymap tree-jumper-active-keymap
  (cond ((not (treesit-parser-list))
         (setq tree-jumper nil)
         (error "No tree-sitter parsers running in this buffer"))
        (tree-jumper
         (tree-jumper-setup))
        (t
         (tree-jumper-remove-overlays)
         (remove-hook 'window-scroll-functions #'tree-jumper-on-scroll t)
         (message "tree-jumper mode disabled"))))

(provide 'tree-jumper)
