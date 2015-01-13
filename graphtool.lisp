;; GRAPH FORMAT: list of lists, which contain points, ids, adjacency list


(ql:quickload "ltk")
(defpackage :graphtool
  (:use :common-lisp :ltk)
  )

(in-package :graphtool)

(defvar *tool-options* '("Vertex" "Undirected" "Directed"))
(defvar *tool* 'vertex)
(defvar *selected-vertex* 'nil)

(defvar *radius* 7)
(defvar *graph* nil)
(defvar *nodes* 0)


(defun main ()
  (setf *graph* nil)
  (setf *tool* 'vertex)
  (setf *radius* 7)
  (setf *nodes* 0)
  (setf *wish-args* '("-name" "GraphTool"))
  (with-ltk ()
    (let* ((menu-frame (make-instance 'frame))
	   (save-button (make-instance 'button
				       :master menu-frame
				       :text "Save"
				       :command
				       (lambda ()
					 nil)))
	   (print-button (make-instance 'button
					:master menu-frame
					:text "Print"
					:command
					(lambda ()
					  (print *graph*))))
	   (exit-button (make-instance 'button
				       :master menu-frame
				       :text "Exit"
				       :command
				       (lambda ()
					 (setf *exit-mainloop* t))))
	   (edge-type-selector (make-instance 'combobox
					      :master menu-frame
					      :text "Edge Type"
					      :values *tool-options*
;					      :command
;					      (lambda (evt) nil)
					      ))
	   (canvas (make-instance 'canvas))
	   )
      (pack menu-frame :side :top)
      (pack exit-button :side :left)
      (pack save-button :side :left)
      (pack print-button :side :left)
      (pack edge-type-selector :side :right)
      (pack canvas :side :bottom :expand t :fill :both)

      (bind canvas "<ButtonPress-1>"
	    (lambda (evt)
	      (cond ((equal "Directed" (text edge-type-selector))
		     (setf *tool* 'directed))
		    ((equal "Undirected" (text edge-type-selector))
		     (setf *tool* 'undirected))
		    (t (setf *tool* 'vertex)))
	      (cond ((eq *tool* 'vertex)
		     (draw-and-add-vertex canvas evt))
		    ((eq *tool* 'undirected)
		     (draw-and-add-edge canvas evt))
		    ((eq *tool* 'directed)
		     (draw-and-add-edge canvas evt))
		    (t nil))))
      
      )))


(defun distance (pt1 pt2)
  (print pt1)
  (print pt2)
  (let ((dx (- (car pt1) (car pt2)))
	(dy (- (cdr pt1) (cdr pt2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun vertex-id (vtx)
  (car vtx))

(defun get-coordinates (vertex)
  (cadr vertex))

(defun find-by-id (id)
  (find (lambda (vtx) (if (eq id (vertex-id vtx)) t nil)) *graph*))

(defun get-adjacent (vtx)
  (caddr vtx))

(defun node-clicked-on (evt)
  (print "nco")
  (let ((evt-pt (cons (event-x evt) (event-y evt))))
    (find (lambda (vtx)
	    (cond ((> (distance evt-pt (get-coordinates vtx)) *radius*)
		   nil)
		  (t t)))
	  *graph*)))

(defun add-vertex (evt)
  (incf *nodes*)
  (push 
   (cons *nodes* (list (cons (event-x evt)
			     (event-y evt))))
   *graph*))

(defun add-edge (evt)
  (let ((node-to-add (node-clicked-on evt)))
    (print node-to-add)
    (cond (*selected-vertex*
	   (cond (node-to-add
		  (print "before push:")
		  (print *selected-vertex*)
		  (push (vertex-id node-to-add)
			(caddr *selected-vertex*))
		  (print *selected-vertex*)
		  (cond ((eq *tool-options* 'undirected)
			 (push (vertex-id *selected-vertex*)
			       (caddr node-to-add)))
			(t nil)))
		 (t nil))
	   (setf *selected-vertex* nil))
	  (t (setf *selected-vertex* node-to-add))))
  )


(defun draw-and-add-vertex (canv evt)
  (add-vertex evt)
  (let ((point (create-oval canv
			    (- (event-x evt) *radius*)
			    (- (event-y evt) *radius*)
			    (+ (event-x evt) *radius*)
			    (+ (event-y evt) *radius*))))
    (itemconfigure canv point :outline :red)
    (itemconfigure canv point :fill :blue)))
    
(defun draw-and-add-edge (canv evt)
  (print "hi")
  (cond (*selected-vertex*
	 (let* ((start (get-coordinates *selected-vertex*))
		(end (cons (event-x evt) (event-y evt)))
		(line (create-line canv (list (car start)
					      (cdr start)
					      (car end)
					      (cdr end)))))
	   (itemconfigure canv line :outline :green)
	   (add-edge evt)))
	(t (add-edge evt))
	))
