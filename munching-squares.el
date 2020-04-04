;;; munching-squares.el --- Munching Squares for Emacs.

;;; Commentary:

;; The original PDP-1 munching squares formula is:
;;
;; lat         /Load test word.
;; add v       /Add from V.
;; dac v       /Deposit back to V.
;; rcl 9s      /Rotate combined AC-IO 9 bits to the left
;; xor v       /Exclusive-or the contents of V.
;; dpy-i 300   /Plot point at x = high ten bits of AC, y = high ten bits of IO.
;;
;; The prevailing defininition from HACKMEM item 147 is:
;;
;; "Munching squares is just views of the graph Y = X XOR T for
;; consecutive values of T = time."
;;
;; I have adopted the latter.  But rather than drawing a complete NxN
;; square for successive T, I compute a list of coordinates to update.

;;; Code:

(require 'cl-macs)

(defun munch-compute (size n list)
  "Compute a sequence of coordinates for Munching Squares.

  SIZE is the size of the square.  N and LIST must be 1 and ((0 0)) when
  not called recursively."
  (cl-loop with half = (/ size 2)
        and result = nil
        for i from 0 below (* n n) by n
        for w = (cl-subseq list i (+ i n)) do
	  (cl-loop for (x y) in w do
	       (push (list x y) result)
	       (push (list (+ x half) (+ y half)) result))
	  (cl-loop for (x y) in w do
	       (push (list (+ x half) y) result)
	       (push (list x (+ y half)) result))
     finally (return  (if (= half 1)
                (nreverse result)
              (munch-compute half (* 2 n) (nreverse result))))))

(defun munch-draw (pixel list)
  "Update each coordinate in LIST with PIXEL."
  (cl-loop for (x y) in list and i from 0 do
       (goto-char (point-min))
       (forward-line (1- y))
       (forward-char (* 2 x))
       (delete-char 2)
       (insert pixel)
       (when (zerop (mod i 10))
         (sit-for .01))))

(defun munching-squares ()
  "Run Munching Squares."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Munching Squares*"))
  (delete-other-windows)
  (erase-buffer)
  (dotimes (i 32)
    (insert "                                                                \n"))
  (insert "\nType Control-G to stop.")
  (let ((list (munch-compute 32 1 '((0 0))))
        (pixels (list "\u2588\u2588" "  " nil)))
    (setf (cdr (cdr pixels)) pixels)
    (cl-loop
       (munch-draw (cl-first pixels) list)
       (setq pixels (cl-rest pixels)))))

(provide 'munching-squares)
;;; munching-squares ends here
