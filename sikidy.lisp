;;;;
;;;;                     SIKIDY
;;;;
;;;;             p04   p03   p02   p01
;;;;
;;;;              •    • •    •     •    p05              ┐
;;;;              •    • •    •     •    p06              │ Renin-tsikidy
;;;;             • •   • •   • •    •    p07              │
;;;;             • •    •    • •    •    p08              ┘
;;;;
;;;; • •   • •   • •    •     •     •    • •   • •       ┐
;;;; • •   • •   • •    •     •     •    • •   • •       │ Zanan-tsikidy
;;;;  •     •    • •   • •   • •    •     •     •        │
;;;; • •   • •   • •   • •    •    • •    •     •        ┘
;;;; p09    p10   p11   p12   p13   p14   p15   p16
;;;; -----------------------------------------------------------------------
;;;; p01			-- tale
;;;; p02			-- maly
;;;; p03			-- fahatelo
;;;; p04			-- bilady
;;;; p05			-- fianahana
;;;; p06			-- abily
;;;; p07			-- alisay
;;;; p08			-- fahavalo
;;;; p09	(p07 + p08)	-- fahasivy
;;;; p10	(p09 + p11)	-- ombiasy
;;;; p11	(p05 + p06)	-- haja
;;;; p12	(p10 + p14)	-- haky
;;;; p13	(p03 + p04)	-- asorità
;;;; p14	(p13 + p15)	-- saily
;;;; p15	(p01 + p02)	-- safary
;;;; p16	(p12 + p01)	-- kiba
;;;; -----------------------------------------------------------------------


(defparameter *renin-tsikidy*
  (list (list nil nil nil nil)
	(list nil nil nil nil)
	(list nil nil nil nil)
	(list nil nil nil nil)))

(defparameter *zanan-tsikidy*
  (list (list nil nil nil nil nil nil nil nil)
	(list nil nil nil nil nil nil nil nil)
	(list nil nil nil nil nil nil nil nil)
	(list nil nil nil nil nil nil nil nil)))

(defparameter *anarana*
  (list
   'bilady	(list 'reny	'mitsangana	0)
   'fahatelo	(list 'reny	'mitsangana	1)
   'maly	(list 'reny	'mitsangana	2)
   'tale	(list 'reny	'mitsangana	3)
   'fianahana	(list 'reny	'mitsilany	0)
   'abily	(list 'reny	'mitsilany	1)
   'alisay	(list 'reny	'mitsilany	2)
   'fahavalo	(list 'reny	'mitsilany	3)
   'fahasivy	(list 'zanaka	'mitsangana	0)
   'ombiasy	(list 'zanaka	'mitsangana	1)
   'haja	(list 'zanaka	'mitsangana	2)
   'haky	(list 'zanaka	'mitsangana	3)
   'asorità	(list 'zanaka	'mitsangana	4)
   'saily	(list 'zanaka	'mitsangana	5)
   'safary	(list 'zanaka	'mitsangana	6)
   'kiba	(list 'zanaka	'mitsangana	7)))


(defmacro toerana (tokon-tsikidy)
  `(car (getf *anarana* ,tokon-tsikidy)))

(defmacro fipetraka (tokon-tsikidy)
  `(cadr (getf *anarana* ,tokon-tsikidy)))

(defmacro laharana (tokon-tsikidy)
  `(caddr (getf *anarana* ,tokon-tsikidy)))

(defun toerana-tenany (tokon-tsikidy)
  (case (toerana tokon-tsikidy)
    (reny *renin-tsikidy*)
    (zanaka *zanan-tsikidy*)))


(defun atambaro (tokon-tsikidy1 tokon-tsikidy2)
  " Operations
       |  •  | • •
   ----+-----+----
    •  | • • |  •
   • • |  •  | • •
"
  (mapcar #'(lambda (x y) (if (= x 1) (if (= y 2) x (+ x y)) y))
	  tokon-tsikidy1 tokon-tsikidy2))


(defun asehoy ()
  (labels ((sorato (tokonana)
	     (mapcar #'(lambda (x)
			 (if (null x) " _ "
			     (case x
			       (1 " • ")
			       (t "• •"))))
		     tokonana)))
    (loop for andalana in *renin-tsikidy*
	  do (format t "~a~{~A~^   ~}~%"
		     (make-string 12 :initial-element #\Space)
		     (sorato andalana)))
    (format t "~%")
    (loop for andalana in *zanan-tsikidy*
	  do (format t "~{~A~^   ~}~%" (sorato andalana)))))


(defun atokony (tokon-tsikidy anatiny)
  (case (fipetraka tokon-tsikidy)
    (mitsilany nil) ;; tsy azo atokona ny tokon-tsikidy mitsilany
    (mitsangana    (loop for andalana in (toerana-tenany tokon-tsikidy)
			 for atiny in anatiny
			 do (setf (elt andalana (laharana tokon-tsikidy)) atiny)))))


(defun omeo (tokon-tsikidy)
  (case (fipetraka tokon-tsikidy)
    (mitsilany (elt (toerana-tenany tokon-tsikidy) (laharana tokon-tsikidy)))
    (mitsangana (loop for andalana in (toerana-tenany tokon-tsikidy)
		      collect (elt andalana (laharana tokon-tsikidy))))))


(defmacro ohatra ()
  `(progn
     (atokony 'tale '(1 1 1 1))
     (atokony 'maly '(1 1 2 2))
     (atokony 'fahatelo '(2 2 2 1))
     (atokony 'bilady '(1 1 2 2))
     (atokony 'fahasivy (atambaro (omeo 'alisay) (omeo 'fahavalo)))
     (atokony 'haja (atambaro (omeo 'fianahana) (omeo 'abily)))
     (atokony 'asorità (atambaro (omeo 'fahatelo) (omeo'bilady)))
     (atokony 'safary (atambaro (omeo 'tale) (omeo 'maly)))
     (atokony 'ombiasy (atambaro (omeo 'fahasivy) (omeo 'haja)))
     (atokony 'saily (atambaro (omeo 'asorità) (omeo 'safary)))
     (atokony 'haky (atambaro (omeo 'ombiasy) (omeo 'saily)))
     (atokony 'kiba (atambaro (omeo 'haky) (omeo 'tale)))
     (asehoy)))
