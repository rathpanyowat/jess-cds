;Jess Program for CDSS Strep A Pharyngitis Treatment
;Nathan Brown and Rath Panyowat 2017

;User will assert prescriptions containing names and medications,
;The CDSS shall output
;1) Whether med is on formulary
;2) Whether patient has an allergy/contraindication
;and if yes, alternatives
;3) Dosages

;Rules (6):
;is desired drug in the formulary? T/F
;does patient have an allergy to med?  T/F
;does patient have a contraindication to med?  T/F
;what is the dosage if penicillin?
;what is the dosage if azithromycin?
;what is the dosage if clindamycin

;Templates(2) (I think we might need 3):
;Patient (Slot: Name, Slot: Allergy, Slot: Contraindications)
;Prescription (Slot: medication, Slot: Name)
;Formulary (Slot: medication(0..*))

;Facts(4)
;Patient 1 (Name, null, Prolonged QT)
;Patient 2 (Name, penicillin, null)
;Patient 3 (Name, penicillin, C diff infection)
;Formulary([penicillin, azithromycin, clindamycin])

;TEMPLATE Definitions
(deftemplate patient
    (slot name)
    (slot allergy)
    (slot past_history)
    (slot medication)
)

;RULE DEFINITIONS

(defrule is_on_formulary
    "Determine if asserted medication is on formulary"
    ?pt <- (patient {(medication == "penicillin") || (medication == "Azithromycin") || (medication == "Clindamycin")})
    =>
    (printout t ?pt.medication" is available on Formulary" crlf)
    (assert (onlist))
)

(defrule is_allergy
    "Check if the patient is allergic to the prescribed drug"
    ?pt <- (patient {allergy == medication})
    =>
    (printout t "Alert!! " ?pt.name " is allergy to the drug " ?pt.medication crlf)
)

(defrule is_contradict
    "Check if the patient is contradiction to the prescribed drug"
    ?pt <- (patient {(past_history == "Prolonged QT") || (past_history == "C. Diff Infection") })
    =>
    (printout t "Alert!! " ?pt.name " is contradiction to the drug " ?pt.medication " due to " ?pt.past_history crlf)
)

;FACT DEFINITIONS:

(deffacts patient_list

    (patient (name "Bob")(past_history "Prolonged QT")(medication "Penicillin"))
    (patient (name "Barbara")(allergy "Penicillin")(medication "Azithromycin"))
    (patient (name "Alice")(allergy "Penicillin")(past_history "C. Diff Infection")(medication "Clindamycin"))
)


;RUNTIME CODE
;############

(reset)
(run)

;#############
;END RUNTIME CODE
