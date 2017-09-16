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

(deftemplate formulary ;Incomplete: Need to figure out how add multiple meds to formulary (0..*)
    (slot medication)
    )

;FACT DEFINITIONS: 
;############## 

(deffacts patient_list ;Can add more patients/detail eventually
  
    (patient (name "Bob")(past_history "Prolonged QT"))
    (patient (name "Barbara")(allergy "Penicillin"))
    (patient (name "Alice")(allergy "Penicillin")(past_history "C. Diff Infection"))
    )

(deffacts formularies ;Incomplete: will need more work to add additional meds to formulary
    (formulary (medication "Penicillin"))
    )

;RULE DEFINITIONS
;################

(defrule is_on_formulary
    ;Determine if asserted medication is on formulary
    ?script <- (prescription)
    ?form <- (formulary{medication == script.medication})
    => 
    (printout t "Available on Formulary" crlf)
    )


;RUNTIME CODE
;############

(reset) 
(assert(prescription(name "Bob")(medication "Penicillin" )))
(run)

;#############
;END RUNTIME CODE

