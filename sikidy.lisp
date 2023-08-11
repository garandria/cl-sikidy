;;
;;                 SIKIDY
;;
;;              p4    p3    p2    p1
;;
;;              •    • •    •     •    p5              ┐
;;              •    • •    •     •    p6              │ Renin-tsikidy
;;             • •   • •   • •    •    p7              │
;;             • •    •    • •    •    p8              ┘
;;
;; • •   • •   • •    •     •     •    • •   • •       ┐
;; • •   • •   • •    •     •     •    • •   • •       │ Zanan-tsikidy
;;  •     •    • •   • •   • •    •     •     •        │
;; • •   • •   • •   • •    •    • •    •     •        ┘
;; p9    p10   p11   p12   p13   p14   p15   p16
;; -------------------------------------
;; p1			-- tale
;; p2			-- maly
;; p3			-- fahatelo
;; p4			-- bilady
;; p5			-- fianahana
;; p6			-- abily
;; p7			-- alisay
;; p8			-- fahavalo
;; p9	(p7 + p8)	-- fahasivy
;; p10	(p9 + p11) 	-- ombiasy
;; p11	(p5 + p6)	-- haja
;; p12	(p10 + p14)	-- haky
;; p13	(p3 + p4)	-- asorità
;; p14	(p13 + p15)	-- saily
;; p15	(p1 + p2)	-- safary
;; p16	(p12 + p1)	-- kiba
;; -------------------------------------


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

