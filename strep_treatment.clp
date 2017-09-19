;Jess Program for CDSS Strep A Pharyngitis Treatment
;Nathan Brown and Rath Panyowat 2017 

;The basic function of the app is that user will assert prescriptions 
;containing names and medications, such as: 
 
;(assert(prescription(name "Bob")(medication "Clindamycin")))

;The CDSS app rules shall output 
;1) Whether the requested med is on the facility formulary for treatment of Strep A
;2) Whether patient has an allergy/contraindication--and if yes, appropriate alternatives 
;3) Whether the requested drug is first line treatment for this indication of Strep A
;4) Recommended adult dosages for the recommended and safe medications 

;TEMPLATE Definitions
(deftemplate patient ;model for each patient
    (slot name)
    (slot allergy)
    (slot past_history)
    )

(deftemplate prescription ;model for a written prescription. Asserted once per cycle by user
    (slot medication)
    (slot name)
    )

(deftemplate med_detail ;model for each medication available
    (slot medication)
    (slot type)
    (slot route)
    (slot contraindication)
    (slot instruction)
    )

;additional miscellaneous CDS logic assertions 
(deftemplate alternatives);will provide alt med list
(deftemplate already_allergy);limit # of hits on allergy IDs
(deftemplate block_original);reformat when Penicillin rec
(deftemplate no_med_provided);when no prescription assertion for medication


;FACT DEFINITIONS: 
;############## 

(deffacts patient_list ;List of system patients. Can add more patients/detail when patients registered
  
    (patient (name "Bob")(past_history "Prolonged QT Interval"))
    (patient (name "Barbara")(allergy "Penicillin"))
    (patient (name "Alice")(allergy "Penicillin")(past_history "C. Diff Infection"))
    (patient (name "Jeff"))
    (patient (name "Zach")(allergy "Azithromycin")(past_history "Prolonged QT Interval"))
    )


(deffacts formulary ;These are the available meds and dosages for Strep A Pharyngitis guideline
    (med_detail (medication "Penicillin")
                (type "Penicillin")
        		(route "IM") 
        		(instruction "Single dose 1,2M Units"))
    (med_detail (medication "Penicillin")
                (type "Penicillin")
        		(route "PO") 
        		(instruction "Take 250mg 4 times a day for 10 days"))
    (med_detail (medication "Amoxicillin")
                (type "Penicillin")
        		(route "PO") 
        		(instruction "Take 500mg 2 times a day for 10 days"))
    (med_detail (medication "Azithromycin")
                (type "Macrolide")
                (route "PO")
        		(instruction "Take 500mg once a day for 5 days")
                (contraindication "Prolonged QT Interval"))
    (med_detail (medication "Clarithromycin")
                (type "Macrolide")
                (route "PO")
        		(instruction "Take 250mg twice a day for 10 days")
                (contraindication "Prolonged QT Interval"))
    (med_detail (medication "Clindamycin")
                (type "Clindamycin")
        		(route "PO")
        		(instruction "Take 300mg three times a day for 10 days")
                (contraindication "C. Diff Infection"))
    (med_detail (medication "Cephalexin")
                (type "Cephalosporin")
        		(route "PO")
        		(instruction "Take 500mg twice a day for 10 days")
                (contraindication "C. Diff Infection"))
    )

;RULE DEFINITIONS
;################

(defrule no_med
    ;User has not asserted a medication for the prescription, 
    ;will give options for the patient
    (declare(salience 10))
    ?script <- (prescription)
    (prescription(medication nil))
    => 
    (assert(alternatives))
    (assert(no_med_provided))
    )

(defrule is_not_on_formulary
    ;Determine if asserted medication is on Strep A formulary
    (declare(salience 9))
    (not(no_med_provided))
    ?script <- (prescription)
    (not(med_detail{(medication == script.medication})))
    => 
    (printout t crlf "'"?script.medication"' is not available on the formulary" crlf)
    (assert(alternatives))
    )

(defrule is_an_allergy
    ;Determine if the asserted patient has allergy to asserted medication
    (declare(salience 9))
    (not(already_allergy));otherwise rule duplicates for each allergy to formulary
    ?script <- (prescription)
    ?patient<- (patient{name == script.name})
    ?excludedmed <-(med_detail{(medication == script.medication)&&((medication == patient.allergy)||(type == patient.allergy))})  
    => 
    (printout t crlf "Caution: "?patient.name" has an allergy to "?excludedmed.medication crlf)
    (assert(alternatives));will provide alternative med list 
    (assert(already_allergy));runs once
    )

(defrule is_contraindicated
    ;Determine if the asserted patient has a contraindication to asserted medication
    (declare(salience 9))
    ?script <- (prescription)
    ?patient<- (patient{name == script.name})
    ?exmed <- (med_detail{(contraindication == patient.past_history)&& (medication == script.medication)})  
    => 
    (printout t crlf "Caution: "?script.name" has a history of "?patient.past_history" which is a contraindication to use of "?exmed.medication crlf)
    (assert(alternatives));will provide alternative med list 
    )


(defrule recommend_penicillin
    ;If asserted patient does not have penicillin allergy, a penicillin is the first line therapy (Recommendation)
    (declare(salience 3))
    (not(alternatives))
    ?script <- (prescription)
    ?patient<- (patient{(name == script.name)&& (allergy != "Penicillin")})
    (med_detail{(medication == script.medication)&&(type != "Penicillin") })
    =>
    (printout t crlf "Best Practice: Penicillins are first line Strep A treatment in absence of Penicillin allergy:" crlf "***************" crlf)
    )

(defrule recommend_penicillin_meds
    ;Similar to above rule, but will provide the penicillin options. 
    (declare(salience 2))
    (not(alternatives))
    ?script <- (prescription)
    ?patient<- (patient{(name == script.name)&& (allergy != "Penicillin")})
    (med_detail{(medication == script.medication)&&(type != "Penicillin") })
     ?detail <- (med_detail{(medication != script.medication)&&(medication != patient.allergy)&& (contraindication != patient.past_history)&&(type == "Penicillin") })
    =>
    (printout t ?detail.medication" "?detail.route": "?detail.instruction crlf)
    (assert(block_original));activates a formating rule
    )

(defrule text_after_penicillin_rec
    ;Reformats the following other options when the penicillin recommendation has fired. 
    (declare(salience 1))
    (not(alternatives))
    (block_original);active with prior rule 
    =>
    (printout t "Otherwise:" crlf)
    )

(defrule the_original_medication_is_ok
    ;Formating rule to give a header to a list of recommendations 
    (declare(salience 1))
    (not(alternatives));original med is recommended 
    (not(block_original));no penicillin recommendation needed 
    =>
    (printout t crlf "Treatment Options and Instructions:" crlf "******************" crlf)
    )

(defrule the_original_medication_instructions
    ;Provides the details for the medication that the provider originally ordered. 
    (declare(salience 0))
    (not(alternatives));original med is recommended 
    ?script <- (prescription)
    ?detail <- (med_detail{medication == script.medication})
    =>
    (printout t ?detail.medication" "?detail.route": "?detail.instruction crlf)
    )

(defrule recommend_another_medication
     ;Provides header formating for a list of alternative medications. 
    (declare(salience 1))
    (alternatives);Alternative medications to provider list are recommended. 
    =>
    (printout t crlf "Recommended alternatives for Strep A" crlf "Pharyngitis Tx in this patient include:" crlf "********************" crlf)
    )

(defrule alternative_medication_options_normal
     ;Provides list of alternative penicillin type meds, when patient with no penicillin allergy but some other medication contraindication
    (declare(salience 0))
    (alternatives);Alternative medications to provider list are recommended. 
    ?script <- (prescription)
    ?patient<- (patient{(name == script.name)&& (allergy != "Penicillin")})
    ?detail <- (med_detail{(medication != script.medication)&&(medication != patient.allergy)&& (contraindication != patient.past_history)&&(type == "Penicillin") })
    =>
    (printout t ?detail.medication" "?detail.route": "?detail.instruction crlf)
    )

(defrule alternative_medication_penicillin_allergic
    ;Provides alternative list of medications when there is a penicillin allergy. 
    (declare(salience 0))
    (alternatives);Alternative medications to provider list are recommended. 
    ?script <- (prescription)
    ?patient<- (patient{(name == script.name)&& (allergy == "Penicillin")})
    ?detail <- (med_detail{(medication != script.medication)&&(type != patient.allergy)&&(medication != patient.allergy)&&(contraindication != patient.past_history) })
    =>
    (printout t ?detail.medication" "?detail.route": "?detail.instruction crlf)
    )

;RUNTIME CODE
;############

(reset) 
;Patients include "Bob","Barbara","Alice","Jeff",and"Zach"
;(assert(prescription(name "Zach")(medication "Penicillin")))

(assert(prescription(name "Zach")(medication "Clindamycin")))
(run)

;#############
;END RUNTIME CODE



