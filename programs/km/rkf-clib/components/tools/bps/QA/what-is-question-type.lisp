;;
;; $Id: what-is-question-type.lisp,v 1.25 2006/10/05 22:56:31 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *concept-exclusion-list* '(|Organic-Structure| |C5H5N-Substance| |C6H5NH2-Substance|
					 |CH3NH2-Substance| |CH3_2_NH-Substance| |CH3_3_N-Substance|
					 |H2NNH2-Substance| |HONH2-Substance| |C6H5OH-Substance|
					 |CH2_COOH_2-Substance| |CH3CH2COOH-Substance| |CH3CHOHCOOH-Substance|
					 |H2C2O4-Substance| |H2C4H4O6-Substance| |H2C6H6O6-Substance|
					 |H2CO3-Substance| |H2O2-Substance| |H2S-Substance| |H2SO3-Substance|
					 |H2SeO3-Substance| |H3AsO3-Substance| |H3AsO4-Substance|
					 |H3BO3-Substance| |H3C6H5O7-Substance| |H3PO4-Substance|
					 |H4P2O7-Substance| |H5IO6-Substance| |HBrO-Substance|
					 |HC2H2O2Cl-Substance| |HCHO2-Substance| |HCN-Substance|
					 |HCNO-Substance| |HClO-Substance| |HF-Substance| |HIO-Substance|
					 |HIO3-Substance| |HN3-Substance| |HNO2-Substance| |BaSO4-Substance|
					 |Ba_NO3_2-Substance| |Ba_OH_2-Substance| |CH3COOH-Substance|
					 |CaCO3-Substance| |CaO-Substance| |Ca_OH_2-Substance| |CsOH-Substance|
					 |CuO-Substance| |CuSO4-Substance| |Cu_CH3COO_2-Substance|
					 |Cu_OH_2-Substance| |Fe_OH_2-Substance| |Fe_OH_3-Substance|
					 |Na2CO3-Substance| |NaHCO3-Substance| |H2SO4-Substance|
					 |H2SiF6-Substance| |HBr-Substance| |HCl-Substance| |HClO3-Substance|
					 |HClO4-Substance| |HI-Substance| |HNO3-Substance| |K2CO3-Substance|
					 |K2CrO4-Substance| |K2SO4-Substance| |KI-Substance| |KOH-Substance|
					 |Li2CO3-Substance| |LiOH-Substance| |MgO-Substance| |MgSO4-Substance|
					 |NH3-Substance| |NH4_3_PO4-Substance| |Na2S-Substance| |NaI-Substance|
					 |NaNO3-Substance| |NaOCl-Substance| |NaOH-Substance| |PbO-Substance|
					 |PbS-Substance| |PbSO4-Substance| |Pb_NO3_2-Substance|
					 |Pb_OH_2-Substance| |CH3NH3Cl-Substance| |NH4Cl-Substance|
					 |NH4NO3-Substance| |NaCl-Substance| |NaF-Substance|
					 |Sn_NO3_2-Substance| |BrO-Minus-Substance| |C2H2O2Cl-Minus-Substance|
					 |C2H5COO-Minus-Substance| |C2O4-Minus-2-Substance|
					 |C3H2O4-Minus-2-Substance| |C3H5O2-Minus-Substance|
					 |C3H5O3-Minus-Substance| |C4H4O6-Minus-2-Substance|
					 |C4H7O2-Minus-Substance| |C6H5O-Minus-Substance|
					 |C6H5O7-Minus-3-Substance| |C6H6O6-Minus-2-Substance|
					 |C7H5O2-Minus-Substance| |CH3COO-Minus-Substance|
					 |CHO2-Minus-Substance| |CN-Minus-Substance| |CNO-Minus-Substance|
					 |CO3-Minus-2-Substance| |ClO-Minus-Substance| |ClO2-Minus-Substance|
					 |CrO4-Minus-2-Substance| |F-Minus-Substance| |H2AsO3-Minus-Substance|
					 |H2AsO4-Minus-Substance| |H2BO3-Minus-Substance|
					 |H2C6H5O7-Minus-Substance| |H2P2O7-Minus-2-Substance|
					 |H2PO4-Minus-Substance| |H3IO6-Minus-2-Substance|
					 |H3P2O7-Minus-Substance| |H4IO6-Minus-Substance|
					 |HAsO4-Minus-2-Substance| |HC2O4-Minus-Substance|
					 |HC3H2O4-Minus-Substance| |HC4H4O6-Minus-Substance|
					 |HC6H5O7-Minus-2-Substance| |HC6H6O6-Minus-Substance|
					 |HCO3-Minus-Substance| |HCrO4-Minus-Substance| |HO2-Minus-Substance|
					 |HPO4-Minus-2-Substance| |HS-Minus-Substance| |HSO3-Minus-Substance|
					 |HSO4-Minus-Substance| |HSeO3-Minus-Substance| |HSeO4-Minus-Substance|
					 |IO-Minus-Substance| |N3-Minus-Substance| |NO2-Minus-Substance|
					 |PO4-Minus-3-Substance| |S-Minus-2-Substance| |SO3-Minus-2-Substance|
					 |SO4-Minus-2-Substance| |SeO3-Minus-2-Substance|
					 |SeO4-Minus-Substance| |CH3NH3-Plus-Substance| |H2NNH3-Plus-Substance|
					 |H3O-Plus-Substance| |NH4-Plus-Substance| |Al-Substance|
					 |Au-Substance| |Ba-Substance| |Be-Substance| |Bh-Substance|
					 |Bi-Substance| |Ca-Substance| |Cd-Substance| |Co-Substance|
					 |Cr-Substance| |Cs-Substance| |Cu-Substance| |Db-Substance|
					 |Fe-Substance| |Fr-Substance| |Ga-Substance| |Hf-Substance|
					 |Hg-Substance| |Hs-Substance| |In-Substance| |Ir-Substance|
					 |K-Substance| |Li-Substance| |Lr-Substance| |Lu-Substance|
					 |Mg-Substance| |Mn-Substance| |Mo-Substance| |Mt-Substance|
					 |Na-Substance| |Nb-Substance| |Ni-Substance| |Os-Substance|
					 |Pb-Substance| |Re-Substance| |Rf-Substance| |Rh-Substance|
					 |Ru-Substance| |Sc-Substance| |Sg-Substance| |Sn-Substance|
					 |Sr-Substance| |Ta-Substance| |Tc-Substance| |Ti-Substance|
					 |Tl-Substance| |V-Substance| |W-Substance| |Y-Substance|
					 |Zn-Substance| |Zr-Substance| |At-Substance| |B-Substance|
					 |Ge-Substance| |Sb-Substance| |Si-Substance| |Te-Substance|
					 |NaHCO3-Solution| |CH3CH2OH-Substance| |CH3OH-Substance|
					 |CO2-Substance| |H2-Substance| |NO-Substance| |NO2-Substance|
					 |O2-Substance| |SO2-Substance| |VOCl-Substance| |P-Substance|
					 |S-Substance| |Po-Substance| |Precipitate| |Pt-Substance|
					 |Ra-Substance| |Rb-Substance| |BaCl2| |BaSO4| |Ba_NO3_2| |Ba_OH_2|
					 C2H5NH2 C5H5N C6H5COOH C6H5NH2 C6H5OH CH2_COOH_2 CH3CH2COOH
					 CH3CHOHCOOH CH3COOCH2CH3 CH3COOH |CH3COONa| CH3NH2 CH3_2_NH CH3_3_N
					 |CaCO3| |CaO| |Ca_OCl_2| |Ca_OH_2| |CsOH| |CuO| |CuSO4| |Cu_CH3COO_2|
					 |Cu_OH_2| |Fe_OH_3| H2C2O4 H2C4H4O6 H2C6H6O6 H2CO3 H2NNH2 H2O H2O2 H2S
					 H2SO3 H2SO4 |H2SeO3| |H2SiF6| |H3AsO3| |H3AsO4| H3BO3 H3C6H5O7 H3PO4
					 H4P2O7 H5IO6 |HBr| |HBrO| |HC2H2O2Cl| HCHO2 HCN HCNO |HCl| |HClO|
					 |HClO2| |HClO3| |HClO4| HIO HIO3 HN3 HNO2 HNO3 HONH2 K2CO3 |K2CrO4|
					 K2SO4 KOH |Li2CO3| |LiOH| |MgO| |MgSO4| NH3 NH4_3_PO4 |Na2CO3| |Na2S|
					 |Na2SO4| |NaHCO3| |NaI| |NaNO3| |NaOCl| |NaOH| |PbO| |PbS| |PbSO4|
					 |Pb_NO3_2| |Pb_OH_2| |RbOH| CH3OH CH4 CO2 H2 N2 NO NO2 O2 SO2 H2C2O4
					 H2C4H4O6 H2C6H6O6 H2CO3 H2NNH2 H2O H2O2 H2S H2SO3 H2SO4 |H2SeO3|
					 |H2SiF6| |H3AsO3| |H3AsO4| H3BO3 H3C6H5O7 H3PO4 H4P2O7 H5IO6 |HBr|
					 |HBrO| |HC2H2O2Cl| HCHO2 HCN HCNO |HCl| |HClO| |HClO2| |HClO3| |HClO4|
					 HI HIO HIO3 HN3 HNO2 HNO3 HONH2 |AsO4-Minus-3| |BO3-Minus-3|
					 |Br-Minus| |BrO-Minus| |C2H2O2Cl-Minus| |C2H5COO-Minus| |C2O4-Minus-2|
					 |C3H2O4-Minus-2| |C3H5O2-Minus| |C3H5O3-Minus| |C4H4O6-Minus-2|
					 |C4H7O2-Minus| |C6H5O-Minus| |C6H5O7-Minus-3| |C6H6O6-Minus-2|
					 |C7H5O-Minus| |C7H5O2-Minus| |CH3-Minus| |CH3COO-Minus| |CHO2-Minus|
					 |CN-Minus| |CNO-Minus| |CO3-Minus-2| |Cl-Minus| |ClO-Minus|
					 |ClO2-Minus| |ClO3-Minus| |ClO4-Minus| |Cr2O7-Minus-2| |CrO4-Minus-2|
					 |F-Minus| |H-Minus| |H2AsO3-Minus| |H2AsO4-Minus| |H2BO3-Minus|
					 |H2C6H5O7-Minus| |H2P2O7-Minus-2| |H2PO4-Minus| |H3IO6-Minus-2|
					 |H3P2O7-Minus| |H4IO6-Minus| |HAsO4-Minus-2| |HBO3-Minus-2|
					 |HC2O4-Minus| |HC3H2O4-Minus| |HC4H4O6-Minus| |HC6H5O7-Minus-2|
					 |HC6H6O6-Minus| |HCO3-Minus| |HCrO4-Minus| |HO2-Minus| |HPO4-Minus-2|
					 |HS-Minus| |HSO3-Minus| |HSO4-Minus| |HSeO3-Minus| |HSeO4-Minus|
					 |I-Minus| |IO-Minus| |IO3-Minus| |IO6-Minus-5| |MnO4-Minus|
					 |N-Minus-3| |N3-Minus| |NH2-Minus| |NO2-Minus| |NO2-Plus| |NO3-Minus|
					 |O-Minus-2| |O2-Minus-2| |OCl-Minus| |OH-Minus| |P2O7-Minus-4|
					 |PO4-Minus-3| |S-Minus-2| |SO3-Minus-2| |SO4-Minus-2| |SeO3-Minus-2|
					 |SeO4-Minus-2| |SiF6-Minus-2| |Xe| Y |Zn| |Zr|
					 |Compute-Acid-Compound-from-Chemical-Name| |CH3NH3Cl| NH4NO3
					 |Compute-Acid-Compound-from-Formula| |Compute-Acid-Ion-from-Name|
					 |Compute-Atomic-Formula-from-Nested-Formula|
					 |Compute-Atomic-Weight-from-Chemical| |Compute-Balanced-Equation|
					 |Compute-Chemical-Entity| |Compute-Chemical-Formula-of-Ionic-Compound|
					 |Compute-Chemical-Ion-from-Name|
					 |Compute-Chemical-Name-from-Formula-of-Ionic-Compound|
					 |Compute-Chemical-Name-from-Ion-of-Acid-Compound|
					 |Compute-Complete-Atomic-Chemical-Formula|
					 |Compute-Compound-Computing-Method-from-Formula|
					 |Compute-Compound-Computing-Method-from-Name|
					 |Compute-Compound-from-Chemical-Formula|
					 |Compute-Compound-from-Chemical-Name|
					 |Compute-Compound-from-Common-Formula|
					 |Compute-Compound-from-Common-Name|
					 |Compute-Concentration-Change-Constant|
					 |Compute-Concentration-from-Quantity-and-Volume|
					 |Compute-Element-from-Molecular-Name| |Compute-Element-from-Name|
					 |Compute-Equilibrium-Constant| |Compute-Equilibrium-Expression|
					 |Compute-Equilibrium-Position|
					 |Compute-Ionic-Compound-Parts-from-Nested-Formula|
					 |Compute-Ionic-Compound-from-Chemical-Name|
					 |Compute-Ionic-Compound-from-Formula| |Compute-Maximum|
					 |Compute-Minimum| |Compute-Mole-from-Weight-and-Chemical|
					 |Compute-Molecule-from-Chemical-Name| |Compute-Molecule-from-Formula|
					 |Compute-Nested-Atomic-Formula-from-Formula|
					 |Compute-Nested-Formula-from-Has-Chemical-Formula|
					 |Compute-Qualitative-Maximum| |Compute-Qualitative-Minimum|
					 |Compute-Quantitative-Maximum| |Compute-Quantitative-Minimum|
					 |Compute-Quantity-from-Concentration-and-Volume|
					 |Compute-Quantity-from-Reaction| |Compute-Quantity-of-Chemical-Object|
					 |Compute-Quantity-of-Chemical-Substance|
					 |Compute-Quantity-of-Chemical-in-Chemical|
					 |Compute-Quantity-of-Chemical-in-Chemicals|
					 |Compute-Volume-from-Concentration-and-Quantity| |Al-Plus-3|
					 |Au-Plus-2| |Ba-Plus-2| |Be-Plus-2| |CH3NH3-Plus| |Ca-Plus-2|
					 |Cd-Plus-2| |Co-Plus-2| |Cs-Plus| |Cu-Plus| |Cu-Plus-2| |Fe-Plus-2|
					 |Fe-Plus-3| |Fr-Plus| |Hg-Plus-2| |Hg2-Plus-2| |K-Plus| |K2-Plus-2|
					 |Li-Plus| |Mg-Plus-2| |Mn-Plus-2| |Mn-Plus-3| |NH4-Plus| |Na-Plus|
					 |Ni-Plus-2| |Pb-Plus-2| |Pt-Plus-2| |Ra-Plus-2| |Rb-Plus| |Sn-Plus-2|
					 |Sr-Plus-2| |V-Plus| |Zn-Plus-2| |C2H5NH3-Plus| |C5H5NH-Plus|
					 |C6H5NH3-Plus| |CH3-Plus| |CH3CH2NH3-Plus| |CH3_2_NH2-Plus|
					 |CH3_3_NH-Plus| |Cr-Plus-3| |H-Plus| |H2NNH3-Plus| |H3NOH-Plus|
					 |H3O-Plus| |HONH3-Plus| C5H5N C6H5COOH C6H5NH2 C6H5OH CH3CH2COOH
					 CH3CH2OH CH3CHOHCOOH CH3COOCH2CH3 CH3COOH |CH3COONa| CH3NH2 CH3OH CH4
					 CO2 |Ca| |CaCO3| |CaO|))

(defparameter *slot-exclusion-list*    '(|instance-of|))

(defun hyphenp(input-concept)
  (let ((input (string input-concept)))
    (search "-" input)))

(defun remove-hyphens(input-concept)
  (let* ((input (string input-concept))
	 (hyphen-pos (hyphenp input)))
    (if (null hyphen-pos)
	input-concept
      (remove-hyphens (intern (format nil "~a ~a" (subseq input 0 hyphen-pos) (subseq input (1+ hyphen-pos) (length input))) :km)))))

(defun use-an-prefix-p(concept)
  (let* ((1st-char (subseq (string-downcase (string concept)) 0 1)))
    (or (string-equal 1st-char "a")
	(string-equal 1st-char "e")
	(string-equal 1st-char "i")
	(string-equal 1st-char "o")
	(string-equal 1st-char "u"))))

(defun formulate-question-tail(target)
  (let ((concept (string (remove-hyphens target))))
    (cond ((string= concept (string-downcase concept))
	   (format nil "~a?" concept))
	  ((use-an-prefix-p concept)
	   (format nil "an ~a?" concept))
	  (t (format nil "a ~a?" concept)))))

(defun get-slots-for-concept(concept)
  (let* ((inst (first (km0 `(|a| ,concept)))))
    (remove-irrelevant-slots
     *slot-exclusion-list*
     (mapcar #'(lambda (triple)
		 (triple-relation triple))
	     (gather-graph inst nil 1)))))

(defun remove-irrelevant-slots (slots-to-ignore slot-list)
  (cond ((null slots-to-ignore) slot-list)
	(t (remove-irrelevant-slots (cdr slots-to-ignore)
				    (remove (car slots-to-ignore)
					    slot-list)))))

(defun put-concepts-with-empty-slot-at-back (concept-list)
  (let ((null-concept-list (remove nil (mapcar #'(lambda (concept)
						  (if (null (get-slots-for-concept concept))
						      concept))
					      concept-list))))
    (append (set-difference concept-list null-concept-list) null-concept-list)))    

(defun format-cpl-scenario (stream triple-list)
  (dolist (triple-cluster (convert-triple-cluster-to-km 
			   (cluster-triple-list-by-relevant-instance-of-relation-head triple-list)))
    (format stream "~%~a~%" triple-cluster)))

(defun extract-brief-answer-from-detail-answer (answer-detail)
  (let* ((line-list (excl::split-regexp "<[/]*h3>" answer-detail))
	 (index (position-if #'(lambda (candidate) (string= (excl::replace-regexp (string-downcase candidate) " " "" )  
							    "answer")) line-list)))
    (nth (+ 1 index) line-list)
    ))

(defun compare-two-brief-answers (ans1 ans2) 
  (let ((a1 (excl::replace-regexp 
	     (excl::replace-regexp 
	      (excl::replace-regexp 
	       (excl::replace-regexp 
		(excl::replace-regexp
		 (string-downcase ans1)  
		 (format nil "~C~C" #\Return #\Newline) 
		 "" :newlines-special nil)
		"<h3>" "" :newlines-special nil)
	       " "	  "" :newlines-special nil) 
	      "</body>" "")
	     "</html>" ""))
	(a2 (excl::replace-regexp (string-downcase ans2) " " "" )))
    (string= a1 a2)
    ))

(defun test-what-is-X(concept-list)
  (remove nil
	  (mapcar #'(lambda(concept)
	      (let ((question (format nil "What is ~a" (formulate-question-tail concept))))
		(if (member concept *concept-exclusion-list* :test #'string-equal)
		    nil
		  (ps-attempt-question question))))
	  concept-list)))

(defun generate-what-is-X-question-list(concept-list)
  (remove nil
	  (mapcar #'(lambda(concept)
		      (let ((question (format nil "What is ~a" (formulate-question-tail concept))))
			(if (member concept *concept-exclusion-list* :test #'string-equal)
			    nil
			  question)))
		  concept-list)))

(defun test-what-is-R-of-X-for-concept(stream concept)
  (cond ((member concept *concept-exclusion-list* :test #'string-equal)
        (format stream "<b>~a</b> was excluded from BPS testing." concept))
	((null (get-slots-for-concept concept))
	 (format stream "<b>~a</b> does not have any slots." concept))
	(t
	 (progn 
	   (format stream "<BR>Slots for <b>~a</b>, ~a<BR>" concept (get-slots-for-concept (intern concept :km)))
	   (format stream "<TABLE BORDER=\"1\">")
	   (format stream "<caption align=\"bottom\">Testing of what is R of X questions for ~a</caption>" concept)
	   (format stream "<TR><TH>Question<TH>CPL triples<TH>Answer Detail<TH>Timings</TH></TD>")
	   (generate-row-output-for-regression-table stream 
						     (mapcar #'(lambda (question)
								 (ps-attempt-question question))
							     (formulate-what-is-R-of-X-question-for-concept concept)))
	   (format stream "</TABLE><BR><BR>")))))

(defun test-what-is-R-of-X-for-concept-list(stream concept-list)
  (mapcar #'(lambda(concept)
	      (progn 
	      (test-what-is-R-of-X-for-concept stream concept)
	      (format stream "<BR>")))
	  concept-list))

(defun test-what-is-X-questions(filename header concept-list)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (progn 
      (format stream "<HTML>")
      (format stream "<B>~a</B><BR>" header)
      (format stream "created at ~a<BR><BR>" (get-time-of-day))
      (format stream "<TABLE BORDER=\"1\">")
      (format stream "<TR><TH>Question<TH>CPL triples<TH>Answer Detail<TH>Timings</TH></TD>")
      (ps-iterate-question-list stream "Testing of what is X questions" (generate-what-is-X-question-list concept-list))
      ;(generate-row-output-for-regression-table stream (test-what-is-X concept-list))
      (format stream "</TABLE>")
      (format stream "</HTML>")
      )
    ))

(defun test-what-is-R-of-X-questions(filename header input-concept-list)
  (let ((concept-list (put-concepts-with-empty-slot-at-back input-concept-list)))
    (with-open-file (stream filename
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (progn 
	(format stream "<HTML>")
	(format stream "<B>~a</B><BR>" header)
	(format stream "created at ~a<BR><BR>" (get-time-of-day))
	(test-what-is-R-of-X-for-concept-list stream concept-list)
	(format stream "</TABLE>")
	(format stream "</HTML>")
	)
      )))

#|

(progn 
  (new-situation)
  (test-what-is-X-questions      "what-is-phys-halo.html" 
				 "Testing What-is-X on phys-halo pump-priming concepts"
				 *phys-halo-pump-prime-list*)
  
  (test-what-is-X-questions      "what-is-bio-halo.html" 
				 "Testing What-is-X on bio-halo pump-priming concepts"
				 *bio-halo-pump-prime-list*)
  
  (test-what-is-X-questions      "what-is-chem-halo.html" 
				 "Testing What-is-X on chem-halo pump-priming concepts"
				 *chem-halo-pump-prime-list*)

  (test-what-is-R-of-X-questions "what-is-R-of-X-phys-halo.html" 
				 "Testing What-is-R-of-X questions on phys-halo pump-priming concepts"
				 *phys-halo-pump-prime-list*)
  
  (test-what-is-R-of-X-questions "what-is-R-of-X-bio-halo.html" 
				 "Testing What-is-R-of-X questions on bio-halo pump-priming concepts"
				 *bio-halo-pump-prime-list*)
  
  (test-what-is-R-of-X-questions "what-is-R-of-X-chem-halo.html" 
				 "Testing What-is-R-of-X questions on chem-halo pump-priming concepts"
				 *chem-halo-pump-prime-list*)
  )

;;;Notes
;;;
;;;Examples
;;;
;(ps-attempt-question "what is a tap-water?")
;(ps-attempt-question "what is a reaction?")
;;;
;;;The following is broken, causing a LISP error. 
;;;
;(ps-attempt-question "what is an organic structure?")

|#

(defun formulate-what-is-R-of-X-question-for-concept(target)
  (let ((concept         (remove-hyphens target))
	(question-tail   (formulate-question-tail target))
	(slot-list       (get-slots-for-concept target)))
    (mapcar #'(lambda (slot)
		(format nil "What is the ~a of ~a" slot question-tail))
	    slot-list)))

(defun ps-test-cpl-parse-question-list-output-to-stream(stream header question-list)
  (progn
    (format stream "<HTML>")
    (format stream "<B>~a</B><BR>" header)
    (format stream "created at ~a<BR><BR>" (get-time-of-day))
    (format stream "*use-loosespeak*=~a, *use-matcher*=~a<BR><BR>" *use-loosespeak* *use-matcher*)
    (format stream "<TABLE BORDER=\"1\">")
    (format stream "<TR><TH>Num<TH>Question<TH>CPL triples<TH>KM assertions<TH>Remarks</TD>")
    (print-cpl-parse-answer stream (ps-test-cpl-parse-question-list question-list))
    (format stream "finished at ~A<BR><BR>" (get-time-of-day))
    (format stream "</HTML>")))
  
(defun ps-test-cpl-parse-question-list-output-to-filename(filename header question-list)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (ps-test-cpl-parse-question-list-output-to-stream stream header question-list)))

(defun ps-test-cpl-parse-question-list (question-list)
  (mapcar #'(lambda(input)
	      (let ((q (if (listp input) (car input) input)))
		(multiple-value-bind 
		  (question scenario compute-questions yes-no-questions scenario-frame)
		  (ps-parse-question q)
		  (list question scenario compute-questions yes-no-questions scenario-frame))))
	  question-list))

(defun print-cpl-parse-answer(stream answer-list)
  (dotimes (i (length answer-list))
    (let* ((answer (nth i answer-list))
	   (question             (first   answer))
	   (triple-list          (list (second  answer) 
				       (third   answer)
				       (fourth  answer)))
	   (km-assertion         (fifth   answer)))
      (format stream "<TR>~%")
      (format stream "<TD>~a</TD>" i)
      (format stream "<TD>~a</TD>" question)
      (format stream "<TD>")
      (ps-print-triples stream triple-list)
      (format stream "</TD>")
      (format stream "<TD><PRE>")
      (dolist (assertion km-assertion)
	(format stream "~a~%" assertion))
      (format stream "</PRE></TD>")
      (format stream "<TD></TD>" )
      (format stream "</TR>~%")))
  (format stream "</TABLE>"))

#|
(ps-test-cpl-parse-question-list-output-to-filename "ttt.html" "None" (list (first *PHYSICS-TEST-QUESTION-LIST*)))
|#
