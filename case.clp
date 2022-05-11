(defmodule MAIN (export ?ALL))

;;******************
;;* STATO INIZIALE *
;;******************

(deftemplate MAIN::attribute
   (slot nome)
   (slot value)
   (slot certainty (default 100.0)))

(defrule MAIN::start
  (declare (salience 10000))
  =>
  (set-fact-duplication TRUE)
  (focus DOMANDE SCEGLI-QUALITA QUARTIERI CASE PRINT-RESULTS))

(defrule MAIN::combine-certainties ""
  (declare (salience 100)
           (auto-focus TRUE))
  ?rem1 <- (attribute (nome ?rel) (value ?val) (certainty ?per1))
  ?rem2 <- (attribute (nome ?rel) (value ?val) (certainty ?per2))
  (test (neq ?rem1 ?rem2))
  =>
  (retract ?rem1)
  (modify ?rem2 (certainty (/ (- (* 100 (+ ?per1 ?per2)) (* ?per1 ?per2)) 100))))

;;****************
;;* DEFFUNCTIONS *
;;****************

(deffunction MAIN::ask-question (?question ?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)

;;******************
;;* REGOLE DOMANDE *
;;******************

(defmodule DOMANDE (import MAIN ?ALL) (export ?ALL))

(deftemplate DOMANDE::domanda
   (slot attribute (default ?NONE))
   (slot the-question (default ?NONE))
   (multislot valid-answers (default ?NONE))
   (slot already-asked (default FALSE))
   (multislot precursors (default ?DERIVE)))
   
(defrule DOMANDE::fai-domanda
   ?f <- (domanda (already-asked FALSE)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
   =>
   (modify ?f (already-asked TRUE))
   (assert (attribute (nome ?the-attribute)
                      (value (ask-question ?the-question ?valid-answers)))))

(defrule DOMANDE::precursore-or-continua
   ?f <- (domanda (already-asked FALSE)
                   (precursors ?name is ?value $?rest))
         (attribute (nome ?name) (value ~?value))
   =>
   (if (eq (nth$ 1 ?rest) or) 
    then (modify ?f (precursors (rest$ ?rest))))
)

(defrule DOMANDE::precursore-soddisfatto
   ?f <- (domanda (already-asked FALSE)
                   (precursors ?name is ?value $?rest))
         (attribute (nome ?name) (value ?value))
   =>
   (if (eq (nth$ 1 ?rest) and) 
    then (modify ?f (precursors (rest$ ?rest)))
    else (modify ?f (precursors ?rest)))
   (if (eq (nth$ 1 ?rest) or) 
    then (modify ?f (precursors)))
)

(defrule DOMANDE::precursore-non-soddisfatto
   ?f <- (domanda (already-asked FALSE)
                   (precursors ?name is-not ?value $?rest))
         (attribute (nome ?name) (value ~?value))
   =>
   (if (eq (nth$ 1 ?rest) and) 
    then (modify ?f (precursors (rest$ ?rest)))
    else (modify ?f (precursors ?rest))))

;;****************
;;* CASE DOMANDE *
;;****************

(defmodule CASE-DOMANDE (import DOMANDE ?ALL))

(deffacts CASE-DOMANDE::domande-attributi
  (domanda (attribute ha-garage)
            (precursors ha-box is no or ha-box is preferisco-no)
            (the-question "Invece un garage? ")
            (valid-answers si no preferisco-si preferisco-no qualsiasi))
  (domanda (attribute ha-box)
            (the-question "Box per l'auto? ")
            (valid-answers si no preferisco-si preferisco-no qualsiasi))
  (domanda (attribute ha-balcone)
            (the-question "Vorresti avere un balcone? ")
            (valid-answers si no preferisco-si preferisco-no qualsiasi))
  (domanda (attribute ha-bagni)
            (the-question "Quanti bagni dovrebbe avere la casa? ")
            (valid-answers 1 2 3 4 qualsiasi))
  (domanda (attribute ha-ascensore)
            (precursors casa-piano is alto or casa-piano is preferisco-alto)
            (the-question "Vorreste avere anche un ascensore?")
            (valid-answers si no preferisco-si preferisco-no qualsiasi))
  (domanda (attribute casa-piano)
            (precursors casa-indipendente is no or casa-indipendente is preferisco-no)
            (the-question "A che piano deve essere la casa? ")
            (valid-answers alto basso terra preferisco-alto preferisco-basso preferisco-terra qualsiasi))
  (domanda (attribute casa-indipendente)
            (the-question "Preferisci una casa indipendente? ")
            (valid-answers si no preferisco-si preferisco-no qualsiasi))
  (domanda (attribute casa-metriquadri)
            (the-question "Di quanti metri quadri vuoi la casa? ")
            (valid-answers 30 40 50 60 70 80 90 100 120 140 160 180 200 250 300 400 500))
  (domanda (attribute casa-zona)
            (the-question "In che zona vuoi comprare la casa? ")
            (valid-answers centro periferia prima-cintura preferisco-centro preferisco-prima-cintura preferisco-periferia qualsiasi))
  ;(domanda (attribute prezzo-massimo)
  ;          (the-question "Quanto vuoi spendere come massimo? ")
  ;          (valid-answers 50000 80000 100000 120000 150000 180000 200000 250000 300000 500000 1000000))
)


;;*******************************
;;* SCEGLI QUALITA' CASE        *
;;*******************************


(defmodule SCEGLI-QUALITA (import DOMANDE ?ALL)
                            (import MAIN ?ALL))

; Scelta zona migliore

(defrule SCEGLI-QUALITA::best-zona-preferisco
            (attribute (nome casa-zona) (value ?value & ~qualsiasi & ~centro & ~periferia & ~prima-cintura))
      =>
            (if (eq ?value preferisco-centro)
             then (assert (attribute (nome best-zona) 
                          (value centro)
                          (certainty 80.0)))
                  (assert (attribute (nome best-zona) 
                          (value periferia)
                          (certainty 20.0)))
                  (assert (attribute (nome best-zona) 
                          (value prima-cintura)
                          (certainty 20.0)))
            )
            (if (eq ?value preferisco-periferia)
             then (assert (attribute (nome best-zona) 
                          (value periferia)
                          (certainty 80.0)))
                  (assert (attribute (nome best-zona) 
                          (value centro)
                          (certainty 20.0)))
                  (assert (attribute (nome best-zona) 
                          (value prima-cintura)
                          (certainty 20.0)))
            )
            (if (eq ?value preferisco-prima-cintura)
             then (assert (attribute (nome best-zona) 
                          (value prima-cintura)
                          (certainty 80.0)))
                  (assert (attribute (nome best-zona) 
                          (value centro)
                          (certainty 20.0)))
                  (assert (attribute (nome best-zona) 
                          (value periferia)
                          (certainty 20.0)))
            )
)

(defrule SCEGLI-QUALITA::best-zona-sicuro
            (attribute (nome casa-zona) (value ?value & ~qualsiasi & ~preferisco-prima-cintura & ~preferisco-centro & ~preferisco-periferia))
      =>
            (assert (attribute (nome best-zona) 
                     (value centro)))
)

(defrule SCEGLI-QUALITA::best-zonaqualsiasi
            (attribute (nome casa-zona) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-zona) 
                     (value centro)
                     (certainty 20.0)))
            (assert (attribute (nome best-zona) 
                     (value periferia)
                     (certainty 20.0)))
            (assert (attribute (nome best-zona) 
                     (value prima-cintura)
                     (certainty 20.0)))
)

; Scelta metri quadri migliore

(defrule SCEGLI-QUALITA::best-metriquadri
            (attribute (nome casa-metriquadri) (value ?value))
      =>
            (assert (attribute (nome best-metriquadri) 
                     (value ?value)
                     (certainty 100.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value (- ?value 10))
                     (certainty 50.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value (+ ?value 10))
                     (certainty 50.0)))
)

(defrule SCEGLI-QUALITA::best-metriquadri-qualsiasi
            (attribute (nome casa-metriquadri) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-metriquadri) 
                     (value 30)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 40)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 50)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 60)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 70)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 80)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 90)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 100)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 120)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 140)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 160)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 180)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 200)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 250)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 300)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 400)
                     (certainty 20.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value 500)
                     (certainty 20.0)))
)

; Scelta casa indipendente o no

(defrule SCEGLI-QUALITA::best-indipendente-preferisco
            (attribute (nome casa-indipendente) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-indipendente) 
                     (value si)
                     (certainty 80.0)))
                  (assert (attribute (nome best-indipendente) 
                     (value no)
                     (certainty 20.0)))
                  (assert (attribute (nome best-piano) 
                     (value terra)
                     (certainty 50.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 50.0)))
                  (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 50.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value si)
                        (certainty 50.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value no)
                        (certainty 50.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-indipendente) 
                     (value no)
                     (certainty 80.0)))
                  (assert (attribute (nome best-indipendente) 
                     (value si)
                     (certainty 20.0)))
            )              
)

(defrule SCEGLI-QUALITA::best-indipendente-sicuro
            (attribute (nome casa-indipendente) (value ?value & ~qualsiasi & ~preferisco-si & ~preferisco-no))
      =>
            (assert (attribute (nome best-indipendente) 
                     (value ?value)))
            (if (eq ?value si)
             then (assert (attribute (nome best-piano) 
                     (value terra)
                     (certainty 50.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 50.0)))
                  (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 50.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value si)
                        (certainty 50.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value no)
                        (certainty 50.0)))
            )
)

(defrule SCEGLI-QUALITA::best-indipendentequalsiasi
            (attribute (nome casa-indipendente) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-indipendente) 
                     (value si)
                     (certainty 20.0)))
            (assert (attribute (nome best-indipendente) 
                     (value no)
                     (certainty 20.0)))
            (assert (attribute (nome best-piano) 
                     (value terra)
                     (certainty 50.0)))
            (assert (attribute (nome best-piano) 
                     (value alto)
                     (certainty 50.0)))
            (assert (attribute (nome best-piano) 
                     (value basso)
                     (certainty 50.0)))
            (assert (attribute (nome best-ascensore) 
                     (value si)
                     (certainty 50.0)))
            (assert (attribute (nome best-ascensore) 
                     (value no)
                     (certainty 50.0)))
)

; Scelta piano della casa

(defrule SCEGLI-QUALITA::best-piano-preferisco
            (attribute (nome casa-piano) (value ?value & ~qualsiasi & ~alto & ~basso & ~terra))
      =>
            (if (eq ?value preferisco-alto)
             then (assert (attribute (nome best-piano) 
                          (value alto)
                          (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                          (value terra)
                          (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                          (value basso)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-basso)
             then (assert (attribute (nome best-piano) 
                          (value basso)
                          (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                          (value terra)
                          (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                          (value alto)
                          (certainty 30.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 80.0)))
            )
            (if (eq ?value preferisco-terra)
             then (assert (attribute (nome best-piano) 
                          (value terra)
                          (certainty 80.0)))
                  (assert (attribute (nome best-piano) 
                          (value basso)
                          (certainty 30.0)))
                  (assert (attribute (nome best-piano) 
                          (value alto)
                          (certainty 30.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 80.0)))
            )
)

(defrule SCEGLI-QUALITA::best-piano-sicuro
            (attribute (nome casa-piano) (value ?value & ~qualsiasi & ~preferisco-alto & ~preferisco-basso & ~preferisco-terra))
      =>
            (assert (attribute (nome best-piano) 
                     (value ?value)))
            (if (eq ?value basso)
                  then  (assert (attribute (nome best-ascensore) 
                              (value si)
                              (certainty 80.0)))
                        (assert (attribute (nome best-ascensore) 
                              (value no)
                              (certainty 80.0)))
            )
            (if (eq ?value terra)
                  then  (assert (attribute (nome best-ascensore) 
                              (value si)
                              (certainty 80.0)))
                        (assert (attribute (nome best-ascensore) 
                              (value no)
                              (certainty 80.0)))
            )
)

(defrule SCEGLI-QUALITA::best-piano-qualsiasi
            (attribute (nome casa-piano) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-piano) 
                     (value basso)
                     (certainty 20.0)))
            (assert (attribute (nome best-piano) 
                     (value alto)
                     (certainty 20.0)))
            (assert (attribute (nome best-piano) 
                     (value terra)
                     (certainty 20.0)))
            (assert (attribute (nome best-ascensore) 
                     (value si)
                     (certainty 20.0)))
            (assert (attribute (nome best-ascensore) 
                     (value no)
                     (certainty 20.0)))
)

; Scelta ascensore

(defrule SCEGLI-QUALITA::best-ascensore-preferisco
            (attribute (nome ha-ascensore) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 20.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 20.0)))
            )
)

(defrule SCEGLI-QUALITA::best-ascensore-sicuro
            (attribute (nome ha-ascensore) (value ?value & ~qualsiasi & ~preferisco-si & ~preferisco-no))
      =>
            (assert (attribute (nome best-ascensore) 
                          (value ?value)))
)

(defrule SCEGLI-QUALITA::best-ascensore-qualsiasi
            (attribute (nome ha-ascensore) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-ascensore) 
                     (value si)
                     (certainty 20.0)))
            (assert (attribute (nome best-ascensore) 
                     (value no)
                     (certainty 20.0)))
)

; Scelta bagni

(defrule SCEGLI-QUALITA::best-bagni
            (attribute (nome ha-bagni) (value ?value & ~qualsiasi))
      =>
            (assert (attribute (nome best-bagni) 
                     (value ?value)))
            (if (> ?value 1)
             then (assert (attribute (nome best-bagni) 
                     (value (- ?value 1))
                     (certainty 40.0)))
            )
            (assert (attribute (nome best-bagni) 
                     (value (+ ?value 1))
                     (certainty 40.0)))
)

(defrule SCEGLI-QUALITA::best-bagni-qualsiasi
            (attribute (nome ha-bagni) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-bagni) 
                     (value 1)
                     (certainty 20.0)))
            (assert (attribute (nome best-bagni) 
                     (value 2)
                     (certainty 20.0)))
            (assert (attribute (nome best-bagni) 
                     (value 3)
                     (certainty 20.0)))
            (assert (attribute (nome best-bagni) 
                     (value 4)
                     (certainty 20.0)))
)

; Scelta balcone

(defrule SCEGLI-QUALITA::best-balcone-preferisco
            (attribute (nome ha-balcone) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-balcone) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-balcone) 
                          (value no)
                          (certainty 20.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-balcone) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-balcone) 
                          (value si)
                          (certainty 20.0)))
            )
)

(defrule SCEGLI-QUALITA::best-balcone-sicuro
            (attribute (nome ha-balcone) (value ?value & ~qualsiasi & ~preferisco-si & ~preferisco-no))
      =>
            (assert (attribute (nome best-balcone) 
                          (value ?value)))
)

(defrule SCEGLI-QUALITA::best-balcone-qualsiasi
            (attribute (nome ha-balcone) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-balcone) 
                     (value si)
                     (certainty 20.0)))
            (assert (attribute (nome best-balcone) 
                     (value no)
                     (certainty 20.0)))
)

; Scelta box auto

(defrule SCEGLI-QUALITA::best-boxauto-preferisco
            (attribute (nome ha-box) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-boxauto) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-boxauto) 
                          (value no)
                          (certainty 20.0)))
                  (assert (attribute (nome best-garage) 
                          (value si)
                          (certainty 60.0)))
                  (assert (attribute (nome best-garage) 
                          (value no)
                          (certainty 20.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-boxauto) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-boxauto) 
                          (value si)
                          (certainty 20.0)))
            )
)

(defrule SCEGLI-QUALITA::best-boxauto-sicuro
            (attribute (nome ha-box) (value ?value & ~qualsiasi & ~preferisco-si & ~preferisco-no))
      =>
            (assert (attribute (nome best-boxauto) 
                          (value ?value)))
            (if (eq ?value si)
             then (assert (attribute (nome best-garage) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-garage) 
                          (value no)
                          (certainty 20.0)))
            )
)

(defrule SCEGLI-QUALITA::best-boxauto-qualsiasi
            (attribute (nome ha-box) (value ?value & qualsiasi))
      =>
            (assert  (attribute (nome best-boxauto) 
                     (value si)
                     (certainty 20.0)))
            (assert  (attribute (nome best-boxauto) 
                     (value no)
                     (certainty 20.0)))
            (assert  (attribute (nome best-garage) 
                     (value si)
                     (certainty 20.0)))
            (assert  (attribute (nome best-garage) 
                     (value no)
                     (certainty 20.0)))
)

; Scelta box auto

(defrule SCEGLI-QUALITA::best-garage-preferisco
            (attribute (nome ha-garage) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-garage) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-garage) 
                          (value no)
                          (certainty 20.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-garage) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-garage) 
                          (value si)
                          (certainty 20.0)))
            )
)

(defrule SCEGLI-QUALITA::best-garage-sicuro
            (attribute (nome ha-garage) (value ?value & ~qualsiasi & ~preferisco-si & ~preferisco-no))
      =>
            (assert (attribute (nome best-garage) 
                          (value ?value)))
)

(defrule SCEGLI-QUALITA::best-garage-qualsiasi
            (attribute (nome ha-garage) (value ?value & qualsiasi))
      =>
            (assert (attribute (nome best-garage) 
                     (value si)
                     (certainty 20.0)))
            (assert (attribute (nome best-garage) 
                     (value no)
                     (certainty 20.0)))
)

;;*************************
;;* REGOLE SELEZIONE ZONE *
;;*************************

(defmodule QUARTIERI (export ?ALL))

(deftemplate QUARTIERI::quartiere
  (slot nome (default ?NONE))
  (slot costo-mq (default any))
  (multislot servizi (default any))
)

(deffacts QUARTIERI::lista-quartieri
      (quartiere (nome parella) (costo-mq 1800) (servizi parco scuola ospedale metro bus supermercato))
      (quartiere (nome barriera-milano) (costo-mq 1000) (servizi parco scuola ospedale bus supermercato piscina centro-commerciale))
      (quartiere (nome crocetta) (costo-mq 2500) (servizi ospedale bus metro supermercato palestra))
      (quartiere (nome mirafiori-nord) (costo-mq 2000) (servizi piscina parco bus metro supermercato palestra))
      (quartiere (nome centro) (costo-mq 5000) (servizi piscina parco bus metro supermercato palestra centro-commerciale))
      (quartiere (nome san-salvario) (costo-mq 2000) (servizi parco bus metro supermercato ospedale))
)

;;*************************
;;* REGOLE SELEZIONE CASE *
;;*************************

(defmodule CASE (import MAIN ?ALL)
                (import QUARTIERI ?ALL))

(deffacts any-attributes
  (attribute (nome best-metriquadri) (value any))
  (attribute (nome best-vani) (value any))
  (attribute (nome best-servizi) (value any))
  (attribute (nome best-piano) (value any))
  (attribute (nome best-citta) (value any))
  (attribute (nome best-zona) (value any))
  (attribute (nome best-quartiere) (value any))
  (attribute (nome best-ascensore) (value any))
  (attribute (nome best-boxauto) (value any))
  (attribute (nome best-terrazzino) (value any))
  (attribute (nome best-balcone) (value any))
  (attribute (nome best-prezzo) (value any))
  (attribute (nome best-indipendente) (value any))
  (attribute (nome best-bagni) (value any))
)

(deftemplate CASE::casa
  (slot nome (default ?NONE))
  (slot metriquadri (default any))
  (slot vani (default any))
  (slot servizi (default any))
  (multislot piano (default any))
  (slot citta (default any))
  (slot zona (default any))
  (slot quartiere (default any))
  (slot ascensore (default any))
  (multislot boxauto (default any))
  (slot garage (default any))
  (slot terrazzino (default any))
  (slot balcone (default any))
  (slot prezzo (default any))
  (slot indipendente (default any))
  (slot bagni (default any))
)


(deffacts CASE::casa-lista 
  (casa (nome casaCentro) (metriquadri 50) (vani 2) (piano basso 1) (citta torino) (zona centro) (quartiere centro) 
        (ascensore no) (boxauto no) (garage no) (terrazzino no) (balcone si) (indipendente no) (bagni 1))
  (casa (nome casaPeriferia) (metriquadri 60) (vani 2) (piano alto 6) (citta torino) (zona periferia) (quartiere barriera-milano) 
        (ascensore si) (boxauto si 15) (garage no) (terrazzino si) (balcone si) (indipendente no) (bagni 1))
  (casa (nome villaCentro) (metriquadri 250) (vani 7) (citta torino) (zona centro) (quartiere centro) 
        (boxauto no) (garage si) (terrazzino si) (balcone si) (indipendente si) (bagni 3))
  (casa (nome casaParella) (metriquadri 60) (vani 5) (piano basso 2) (citta torino) (zona prima-cintura) (quartiere parella) 
        (ascensore no) (boxauto si 15) (garage no) (terrazzino no) (balcone si) (indipendente no) (bagni 1))
  (casa (nome casaParella2) (metriquadri 100) (vani 2) (citta torino) (zona prima-cintura) (quartiere parella) 
        (garage si) (balcone si) (indipendente si) (bagni 2))
  (casa (nome casaPeriferia2) (metriquadri 80) (vani 2) (piano alto 5) (citta torino) (zona periferia) (quartiere san-salvario) 
        (garage no) (boxauto si 25) (balcone si) (terrazzino si) (indipendente no) (bagni 1))
  (casa (nome casaPeriferiaMira) (metriquadri 140) (vani 3) (citta torino) (zona prima-cintura) (quartiere mirafiori-nord) 
        (garage si) (balcone si) (terrazzino si) (indipendente si) (bagni 2))
  (casa (nome casaPeriferiaMira2) (metriquadri 40) (vani 2) (piano terra 0) (citta torino) (zona prima-cintura) (quartiere mirafiori-nord) 
        (garage no) (balcone si) (terrazzino no) (indipendente no) (bagni 1))
  (casa (nome casaCentro2) (metriquadri 70) (vani 1) (citta torino) (zona centro) (quartiere centro) 
        (garage no) (boxauto si 20) (balcone si) (terrazzino si) (indipendente no) (bagni 1))
  (casa (nome casaPeriferia3) (metriquadri 200) (vani 4) (citta torino) (zona periferia) (quartiere san-salvario) 
        (garage si) (balcone si) (indipendente si) (bagni 2))
  (casa (nome casaPeriferia4) (metriquadri 120) (vani 2) (citta torino) (zona periferia) (quartiere barriera-milano) 
        (garage no) (boxauto si 35) (terrazzino no) (indipendente si) (bagni 2))
  (casa (nome casaCrocetta) (metriquadri 90) (vani 3) (citta torino) (zona prima-cintura) (quartiere crocetta) 
        (garage si) (boxauto no) (balcone si) (indipendente si) (bagni 2))
  (casa (nome casaCrocetta) (metriquadri 70) (vani 2) (piano basso 1) (citta torino) (zona prima-cintura) (quartiere crocetta) 
        (garage si) (boxauto no) (balcone no) (terrazzino no) (indipendente no) (bagni 1))
)

(defrule CASE::genera-prezzo
   (declare (salience 10))
   ?f <- (casa (prezzo any) (metriquadri ?mq) (quartiere ?q) (boxauto $? ?b ?bmq $?) (garage ?g))
         (quartiere (nome ?q) (costo-mq ?cmq) (servizi $?s))
   =>    (modify ?f (prezzo (+ (+ (+ (* ?cmq ?mq) 30000) (* (length$ $?s) 5000)) (* ?bmq ?cmq))))
         (modify ?f (servizi (length$ $?s)))         
)

(defrule CASE::genera-prezzo-any
   (declare (salience 10))
   ?f <- (casa (prezzo any) (metriquadri ?mq) (quartiere ?q) (boxauto $? ?b $?) (garage ?g))
         (quartiere (nome ?q) (costo-mq ?cmq) (servizi $?s))
   =>    (if (eq ?g si)
          then (modify ?f (prezzo (+ (+ (+ (* ?cmq ?mq) 30000) (* (length$ $?s) 5000)) 40000)))
         )
         (if (eq ?g no)
          then (modify ?f (prezzo (+ (+ (* ?cmq ?mq) 30000) (* (length$ $?s) 5000))))
         )
         (modify ?f (servizi (length$ $?s)))
)
  
(defrule CASE::genera-case
  (casa (nome ?nome)
        (metriquadri ?mq)
        (vani ?vani)
        (servizi ?serv)
        (piano $? ?pianoAltezza $?)
        (citta ?citta)
        (zona ?zona)
        (quartiere ?quart)
        (ascensore ?asce)
        (boxauto $? ?box $?)
        (garage ?garage)
        (terrazzino ?terraz)
        (balcone ?balcone)
        (prezzo ?prezzo)
        (indipendente ?indip)
        (bagni ?bagni)
  )
  (attribute (nome best-metriquadri) (value ?mq) (certainty ?certainty-1))
  (attribute (nome best-piano) (value ?pianoAltezza) (certainty ?certainty-4))
  (attribute (nome best-zona) (value ?zona) (certainty ?certainty-6))
  (attribute (nome best-boxauto) (value ?box) (certainty ?certainty-9))
  (attribute (nome best-indipendente) (value ?indip) (certainty ?certainty-14))
  (attribute (nome best-garage) (value ?garage) (certainty ?certainty-15))
  (attribute (nome best-balcone) (value ?balcone) (certainty ?certainty-16))
  (attribute (nome best-ascensore) (value ?asce) (certainty ?certainty-17))
  (attribute (nome best-bagni) (value ?bagni) (certainty ?certainty-18))
  =>
  (assert (attribute (nome casa) (value ?nome)
                (certainty (min ?certainty-1 ?certainty-16 ?certainty-4
                                ?certainty-6 ?certainty-15 ?certainty-17
                                ?certainty-9  ?certainty-14 ?certainty-18
                          ))))
  )

;;*****************************
;;* STAMPA RISULTATI CASE     *
;;*****************************

(defmodule PRINT-RESULTS (import MAIN ?ALL))

(defrule PRINT-RESULTS::header ""
   (declare (salience 10))
   =>
   (printout t t)
   (printout t "        CASE SELEZIONATE" t t)
   (printout t " CASA                  CERTAINTY" t)
   (printout t " -------------------------------" t)
   (assert (phase print-casa)))

(defrule PRINT-RESULTS::print-casa ""
  ?rem <- (attribute (nome casa) (value ?nome) (certainty ?per))		  
  (not (attribute (nome casa) (certainty ?per1&:(> ?per1 ?per))))
  =>
  (retract ?rem)
  (format t " %-24s %2d%%%n" ?nome ?per))

(defrule PRINT-RESULTS::remove-poor-casa-choices ""
  ?rem <- (attribute (nome casa) (certainty ?per&:(< ?per 20)))
  =>
  (retract ?rem))

(defrule PRINT-RESULTS::end-spaces ""
   (not (attribute (nome casa)))
   =>
   (printout t t))