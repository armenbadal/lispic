

(defun new-shift (dx dy)
  (list (list 1.0 0.0 dx)
	(list 0.0 1.0 dy)
	(list 0.0 0.0 1.0)))

(defun new-rotate (phi)
  (let ((sv (sin phi))
        (cv (cos phi)))
    (list (list cv sv 0.0)
	  (list (- sv) cv 0.0)
	  (list 0.0 0.0 1.0))))

(defun new-scale (sx sy)
  (list (list sx  0.0 0.0)
	(list 0.0 sy  0.0)
	(list 0.0 0.0 1.0)))

(defun matrix-by-vector (mx vc)
  (mapcar #'(lambda (r) (apply #'+ (mapcar #'* r vc))) mx))

(defgeneric apply-transform (obj mx))

(defmacro shift-by (obj dx dy)
  `(apply-transform ,obj (new-shift ,dx ,dy)))

(defmacro rotate-by (obj angle)
  `(apply-transform ,obj (new-rotate ,angle)))

(defmacro scale-by (obj sx sy)
  `(apply-transform ,obj (new-scale ,sx ,sy)))


(defstruct point x y)

(defmethod apply-transform ((obj point) mx)
  (let ((resv (matrix-by-vector mx (list (point-x obj) (point-y obj) 1.0))))
    (setf (point-x obj) (first resv)
          (point-y obj) (second resv))
    obj))


(defstruct segment begin end)

(defmethod apply-transform ((obj segment) mx)
  (apply-transform (segment-begin obj) mx)
  (apply-transform (segment-end obj) mx)
  obj)


(defstruct circle center radius)

(defmethod apply-transform ((obj circle) mx)
  (apply-transform (circle-center obj) mx)
  obj)



(defgeneric postscript (obj))

(defmethod postscript ((obj point))
  (format nil "~a ~a" (point-x obj) (point-y obj)))

(defmethod postscrip ((obj segment))
  (format nil "~a moveto ~a lineto"
          (postscript (segment-begin obj))
          (postscript (segment-end obj))))

(defmethod postscript ((obj circle))
  (format nil "~a ~a 0 360 arc closepath"
          (postscript (circle-center obj))
          (circle-radius obj)))


