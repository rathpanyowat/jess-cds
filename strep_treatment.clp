;Jess Program for CDSS Strep A Pharyngitis Treatment
;Nathan Brown and Rath Panyowat 2017 

;User will assert prescriptions containing names and medications, 
;The CDSS shall output 
;1) Whether med is on formulary 
;2) Whether patient has an allergy/contraindication 
;and if yes, alternatives 
;3) Dosages  

;TEMPLATE Definitions
(deftemplate patient 
    (slot name)
    (slot allergy)
    (slot past_history)
    )

(deftemplate prescription ;A script will be asserted
    (slot medication)
    (slot name)
    )

(deftemplate formulary ;Multiple meds using multislot
    (multislot medications)
    )

;FACT DEFINITIONS: 
;############## 

(deffacts patient_list ;Can add more patients/detail eventually
  
    (patient (name "Bob")(past_history "Prolonged QT Interval"))
    (patient (name "Barbara")(allergy "Penicillin"))
    (patient (name "Alice")(allergy "Penicillin")(past_history "C. Diff Infection"))
    )

(deffacts formularies ;Multiple meds using multislot
    (formulary (medications "Penicillin" "Clindamycin" "Azithromycin"))
   
    )

;RULE DEFINITIONS
;################

(defrule is_not_on_formulary
    ;Determine if asserted medication is on formulary
    ?script <- (prescription)

    (not(formulary(medications $? ?script.medication $? )))
    => 
    (printout t "'"?script.medication"' is not available on the formulary" crlf)
    )

(defrule patient_has_an_allergy
    ;Determine if the asserted patient has allergy to asserted medication
    ?script <- (prescription)
    (patient{(name == script.name)&&(allergy == script.medication)})
    => 
    (printout t ?script.name" has an allergy to "?script.medication crlf)
 
    )

(defrule patient_has_a_contraindication
    ;Determine if the asserted patient has a contraindication to asserted medication
    ?script <- (prescription)
    ?patient<- (patient{name == script.name})
    (or
    (and 
        (patient{(name == script.name)&&(past_history == "Prolonged QT Interval")})
        (prescription{medication == "Azithromycin"}))
    (and 
        (patient{(name == script.name)&&(past_history == "C. Diff Infection")})
        (prescription{medication == "Clindamycin"})) 
    )  
    => 
    (printout t ?script.name" has a history of "?patient.past_history" which is a contraindication to use of "?script.medication crlf)
    )




;RUNTIME CODE
;############

(reset) 
(assert(prescription(name "Alice")(medication "Penicillin")))
(run)

;#############
;END RUNTIME CODE

