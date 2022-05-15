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
  (set-strategy depth)
  (set-fact-duplication TRUE)
  (focus GENERA-PREZZI DOMANDE SCEGLI-QUALITA QUARTIERI CASE PRINT-RESULTS DOMANDE2 SCEGLI-QUALITA CASE PRINT-RESULTS DOMANDE3 MODIFICA-PREFERENZE CASE PRINT-RESULTS)
)

(defrule MAIN::combine-certainties ""
  (declare (salience 200)
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
   (slot giro (default ?NONE))
   (slot the-question (default ?NONE))
   (multislot valid-answers (default ?NONE))
   (slot already-asked (default FALSE))
   (multislot precursors (default ?DERIVE)))
   
(defrule DOMANDE::fai-domanda
   ?f <- (domanda (already-asked FALSE)
                   (giro 1)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
   =>
   (modify ?f (already-asked TRUE))
   (assert (attribute (nome ?the-attribute)
                      (value (ask-question ?the-question ?valid-answers))))
)

(defrule DOMANDE::rifai-domanda
   ?f <- (domanda (already-asked TRUE)
                   (giro 1)
                   (precursors)
                   (attribute ?the-attribute))
         (attribute (nome ?the-attribute) (value qualsiasi))
   =>
   (modify ?f (already-asked REDO))
)

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

(defmodule CASE-DOMANDE (import DOMANDE ?ALL) (export ?ALL))

(deffacts CASE-DOMANDE::domande-attributi
  (domanda (attribute modifiche)
            (giro 3)
            (the-question "Vuoi modificare una preferenza? ")
            (valid-answers 
                  zona-centro zona-periferia zona-prima-cintura
                  metri-30 metri-40 metri-50 metri-60 metri-70 metri-80 metri-90 metri-100 metri-120 metri-140 metri-160 metri-180 metri-200 metri-250 metri-300 metri-400 metri-500
                  indipendente-si indipendente-no
                  piano-alto piano-basso piano-terra
                  ascensore-si ascensore-no
                  bagni-1 bagni-2 bagni-3 bagni-4
                  balcone-si balcone-no
                  boxauto-si boxauto-no
                  garage-si garage-no
                  no
            )
  )
  (domanda (attribute ha-cane)
            (giro 2)
            (the-question "Hai un cane? ")
            (valid-answers si no))
  (domanda (attribute fa-aperitivi)
            (giro 2)
            (the-question "Sei solito ad invitare amici per mangiare o per fare aperitivi? ")
            (valid-answers si no ogni-tanto))
  (domanda (attribute fa-pendolare)
            (giro 2)
            (the-question "Fai il pendolare, ti sposti in Torino tramite i mezzi o usi la macchina? ")
            (valid-answers pendolare torino macchina))
  (domanda (attribute fuma)
            (giro 2)
            (the-question "Fumi? ")
            (valid-answers si no))
  (domanda (attribute fa-shopping)
            (giro 2)
            (the-question "Ami fare shopping? ")
            (valid-answers si no ogni-tanto))
  (domanda (attribute ha-bambini)
            (giro 2)
            (the-question "Hai dei bambini? ")
            (valid-answers si no))
  (domanda (attribute sono-anziani)
            (giro 2)
            (the-question "Siete anziani? ")
            (valid-answers si no))
  (domanda (attribute prezzo-massimo)
            (giro 1)
            (the-question "Quanto vuoi spendere come massimo? ")
            (valid-answers 50000 80000 100000 120000 150000 180000 200000 250000 300000 500000 1000000))
  (domanda (attribute ha-garage)
            (giro 1)
            (precursors ha-box is no or ha-box is preferisco-no)
            (the-question "Invece un garage? ")
            (valid-answers si no preferisco-si preferisco-no qualsiasi))
  (domanda (attribute ha-box)
            (giro 1)
            (the-question "Box per l'auto? ")
            (valid-answers si no preferisco-si preferisco-no qualsiasi))
  (domanda (attribute ha-balcone)
            (giro 1)
            (the-question "Vorresti avere un balcone? ")
            (valid-answers si no preferisco-si preferisco-no qualsiasi))
  (domanda (attribute ha-bagni)
            (giro 1)
            (the-question "Quanti bagni dovrebbe avere la casa? ")
            (valid-answers 1 2 3 4))
  (domanda (attribute ha-ascensore)
            (giro 1)
            (precursors casa-piano is alto or casa-piano is preferisco-alto)
            (the-question "Vorreste avere anche un ascensore? ")
            (valid-answers si no preferisco-si preferisco-no qualsiasi))
  (domanda (attribute casa-piano)
            (giro 1)
            (precursors casa-indipendente is no or casa-indipendente is preferisco-no)
            (the-question "A che piano deve essere la casa? ")
            (valid-answers alto basso terra preferisco-alto preferisco-basso preferisco-terra qualsiasi))
  (domanda (attribute casa-indipendente)
            (giro 1)
            (the-question "Preferisci una casa indipendente? ")
            (valid-answers si no preferisco-si preferisco-no qualsiasi))
  (domanda (attribute casa-metriquadri)
            (giro 1)
            (the-question "Di quanti metri quadri vuoi la casa? ")
            (valid-answers 30 40 50 60 70 80 90 100 120 140 160 180 200 250 300 400 500))
  (domanda (attribute casa-zona)
            (giro 1)
            (the-question "In che zona vuoi comprare la casa? ")
            (valid-answers centro periferia prima-cintura preferisco-centro preferisco-prima-cintura preferisco-periferia qualsiasi))
)

;;*******************************
;;* REGOLE DOMANDE SECONDO GIRO *
;;*******************************

(defmodule DOMANDE2     (import MAIN ?ALL) (import DOMANDE ?ALL) (export ?ALL))

(defrule DOMANDE2::prima-domanda-refresh
   ?f <- (domanda (already-asked FALSE)
                   (giro 2)
                   (precursors)
                   (the-question "Siete anziani? "))
   =>
      (refresh PRINT-RESULTS::header)
      (refresh PRINT-RESULTS::print-casa)
      (refresh PRINT-RESULTS::remove-poor-casa-choices)
      (refresh PRINT-RESULTS::end-spaces)
)

(defrule DOMANDE2::fai-domanda2
   ?f <- (domanda (already-asked FALSE)
                   (giro 2)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
   =>
   (modify ?f (already-asked TRUE))
   (assert (attribute (nome ?the-attribute)
                      (value (ask-question ?the-question ?valid-answers))))
)

;;*******************************
;;* REGOLE DOMANDE TERZO GIRO   *
;;*******************************

(defmodule DOMANDE3     (import MAIN ?ALL) (import DOMANDE ?ALL) (export ?ALL))

(defrule DOMANDE3::fai-domanda3
   ?f <- (domanda (already-asked FALSE)
                   (giro 3)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
   =>
   (assert (attribute (nome ?the-attribute)
                      (value (ask-question ?the-question ?valid-answers))))
)

(defrule DOMANDE3::rifai-domanda-finale
   ?f <- (domanda (already-asked FALSE)
                   (giro 3)
                   (precursors)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
         (attribute (nome ?the-attribute) (value ~no))
   =>
   (assert (attribute (nome ?the-attribute)
                      (value (ask-question ?the-question ?valid-answers))))
)

(defrule DOMANDE3::stop-domanda-finale
   ?f <- (domanda (already-asked FALSE)
                   (giro 3)
                   (the-question ?the-question)
                   (attribute ?the-attribute))
   ?a <- (attribute (nome ?the-attribute) (value no))
   =>
   (modify ?f (already-asked TRUE))
   (retract ?a)
   (refresh PRINT-RESULTS::header)
   (refresh PRINT-RESULTS::print-casa)
   (refresh PRINT-RESULTS::remove-poor-casa-choices)
   (refresh PRINT-RESULTS::end-spaces)
   (refresh CASE::genera-case2)
   (set-strategy breadth)
)

(defrule DOMANDE3::rifai-domanda3
   (declare (salience 10))
   ?f <- (domanda (already-asked REDO)
                   (the-question ?the-question)
                   (attribute ?the-attribute)
                   (valid-answers $?valid-answers))
   ?a <- (attribute (nome ?the-attribute))
   =>
   (modify ?f (already-asked TRUE))
   (assert (attribute (nome (sym-cat modifiche- ?the-attribute))
                      (value (ask-question ?the-question ?valid-answers)))
   )
)

;;*************************
;;* REGOLE QUARTIERI      *
;;*************************

(defmodule QUARTIERI (export ?ALL))

(deftemplate QUARTIERI::quartiere
  (slot nome (default ?NONE))
  (slot costo-mq (default any))
  (multislot servizi (default any))
)

(deffacts QUARTIERI::lista-quartieri
      (quartiere (nome parella) (costo-mq 1800) (servizi parco scuola ospedale metro bus stazione supermercato))
      (quartiere (nome barriera-milano) (costo-mq 1000) (servizi parco scuola ospedale bus supermercato piscina centro-commerciale))
      (quartiere (nome crocetta) (costo-mq 2500) (servizi ospedale bus metro supermercato palestra))
      (quartiere (nome mirafiori-nord) (costo-mq 2000) (servizi piscina parco bus metro stazione supermercato palestra))
      (quartiere (nome centro) (costo-mq 5000) (servizi piscina parco bus metro supermercato palestra centro-commerciale))
      (quartiere (nome san-salvario) (costo-mq 2000) (servizi parco bus metro supermercato ospedale))
)

;;*******************************
;;* SCEGLI QUALITA' CASE        *
;;*******************************


(defmodule SCEGLI-QUALITA (import MAIN ?ALL) (import QUARTIERI ?ALL))

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
                     (value ?value)))
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
                     (certainty 70.0)))
            (assert (attribute (nome best-metriquadri) 
                     (value (+ ?value 10))
                     (certainty 70.0)))
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
                     (certainty 30.0)))
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
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 30.0)))
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
            (attribute (nome ha-bagni) (value ?value))
      =>
            (assert (attribute (nome best-bagni) 
                     (value ?value)))
            (if (> ?value 1)
             then (assert (attribute (nome best-bagni) 
                     (value (- ?value 1))
                     (certainty 60.0)))
            )
            (if (< ?value 4)
             then (assert (attribute (nome best-bagni) 
                     (value (+ ?value 1))
                     (certainty 90.0)))
            )
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
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-balcone) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-balcone) 
                          (value si)
                          (certainty 30.0)))
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
                          (certainty 40.0)))
                  (assert (attribute (nome best-garage) 
                          (value no)
                          (certainty 40.0)))
                  (assert (attribute (nome best-garage) 
                          (value si)
                          (certainty 80.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-boxauto) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-boxauto) 
                          (value si)
                          (certainty 30.0)))
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
                          (certainty 100.0)))
                  (assert (attribute (nome best-garage) 
                          (value no)
                          (certainty 30.0)))
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

; Scelta garage

(defrule SCEGLI-QUALITA::best-garage-preferisco
            (attribute (nome ha-garage) (value ?value & ~qualsiasi & ~si & ~no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-garage) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-garage) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-garage) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-garage) 
                          (value si)
                          (certainty 30.0)))
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

; Scelta anziani

(defrule SCEGLI-QUALITA::best-anziani
            (attribute (nome sono-anziani) (value ?value))
            (not (attribute (nome casa-piano) (value ?value2 & alto | basso | terra)))
            (not (attribute (nome ha-ascensore) (value ?value3 & si | no)))
      =>
            (if (eq ?value si)
             then (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 20.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value si)
                        (certainty 70.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value no)
                        (certainty 30.0)))
             else (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 70.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value si)
                        (certainty 70.0)))
                  (assert (attribute (nome best-ascensore) 
                        (value no)
                        (certainty 70.0)))
            )
            
)

(defrule SCEGLI-QUALITA::best-anziani-asce
            (attribute (nome sono-anziani) (value ?value))
            (not (attribute (nome casa-piano) (value ?value2 & alto | basso | terra)))
            (attribute (nome ha-ascensore) (value ?value3 & si | no))
      =>
            (if (eq ?value si)
             then (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 20.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 80.0)))
             else (assert (attribute (nome best-piano) 
                        (value basso)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value alto)
                        (certainty 70.0)))
                  (assert (attribute (nome best-piano) 
                        (value terra)
                        (certainty 70.0)))
            )
            
)

; Scelta bambini

(defrule SCEGLI-QUALITA::best-bambini
            (attribute (nome ha-bambini) (value ?value & si))
            (or   (quartiere (nome ?nq) (servizi $? parco $?))
                  (quartiere (nome ?nq) (servizi $? scuola $? ))
            )
            (not (quartiere (nome ?nq) (servizi $? parco scuola $?)))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 70.0))
            ) 
)

(defrule SCEGLI-QUALITA::best-bambini-parco-scuola
            (attribute (nome ha-bambini) (value ?value & si))
            (quartiere (nome ?nq) (servizi $? parco scuola $?))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 85.0))
            ) 
)

(defrule SCEGLI-QUALITA::best-bambini-zona
            (attribute (nome ha-bambini) (value ?value & si))
            (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))

      =>
            (assert (attribute (nome best-zona) 
                        (value periferia)
                        (certainty 65.0))
            )  
            (assert (attribute (nome best-zona) 
                        (value centro)
                        (certainty 30.0))
            ) 
            (assert (attribute (nome best-zona) 
                        (value prima-cintura)
                        (certainty 65.0))
            )  
)

(defrule SCEGLI-QUALITA::best-bambini-indip
            (attribute (nome ha-bambini) (value ?value & si))
            (not (attribute (nome casa-indipendente) (value ?value2 & si | no)))
      =>
            (assert (attribute (nome best-indipendente) 
                        (value si)
                        (certainty 70.0))
            )
            (assert (attribute (nome best-indipendente) 
                        (value no)
                        (certainty 25.0))
            )
)

(defrule SCEGLI-QUALITA::best-bambini-no
            (attribute (nome ha-bambini) (value ?value & no))
            (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))
      =>
            (assert (attribute (nome best-zona) 
                        (value centro)
                        (certainty 70.0))
            )  
            (assert (attribute (nome best-zona) 
                        (value periferia)
                        (certainty 50.0))
            ) 
            (assert (attribute (nome best-zona) 
                        (value prima-cintura)
                        (certainty 50.0))
            )  
)

(defrule SCEGLI-QUALITA::best-bambini-indip-no
            (attribute (nome ha-bambini) (value ?value & no))
            (not (attribute (nome casa-indipendente) (value ?value2 & si | no)))
      =>
            (assert (attribute (nome best-indipendente) 
                        (value si)
                        (certainty 40.0))
            )
            (assert (attribute (nome best-indipendente) 
                        (value no)
                        (certainty 50.0))
            )  
)

; Scelta shopping

(defrule SCEGLI-QUALITA::best-shopping
            (attribute (nome fa-shopping) (value ?value & si))
            (quartiere (nome ?nq) (servizi $? centro-commerciale $?))
            (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 70.0))
            )
            (assert (attribute (nome best-zona) 
                        (value centro)
                        (certainty 85.0))
            ) 
)

(defrule SCEGLI-QUALITA::best-shopping-sicuro
            (attribute (nome fa-shopping) (value ?value & si))
            (quartiere (nome ?nq) (servizi $? centro-commerciale $?))
            (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 70.0))
            )
)

(defrule SCEGLI-QUALITA::best-shopping-ogni-tanto
            (attribute (nome fa-shopping) (value ?value & ogni-tanto))
            (quartiere (nome ?nq) (servizi $? centro-commerciale $?))
            (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 55.0))
            )
            (assert (attribute (nome best-zona) 
                        (value centro)
                        (certainty 55.0))
            )  
)

(defrule SCEGLI-QUALITA::best-shopping-ogni-tanto-sicuro
            (attribute (nome fa-shopping) (value ?value & ogni-tanto))
            (quartiere (nome ?nq) (servizi $? centro-commerciale $?))
            (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 55.0))
            )
)

(defrule SCEGLI-QUALITA::best-shopping-no
            (attribute (nome fa-shopping) (value ?value & no))
            (quartiere (nome ?nq) (servizi $? centro-commerciale $?))
            (not (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura)))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 30.0))
            )
            (assert (attribute (nome best-zona) 
                        (value centro)
                        (certainty 30.0))
            )  
)

(defrule SCEGLI-QUALITA::best-shopping-no-sicuro
            (attribute (nome fa-shopping) (value ?value & no))
            (quartiere (nome ?nq) (servizi $? centro-commerciale $?))
            (attribute (nome casa-zona) (value ?value2 & centro | periferia | prima-cintura))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 30.0))
            ) 
)

; Scelta fumatore

(defrule SCEGLI-QUALITA::best-fumatore-si
            (attribute (nome fuma) (value ?value & si))
      =>
            (assert (attribute (nome best-balcone) 
                        (value si)
                        (certainty 80.0))
            )
            (assert (attribute (nome best-balcone) 
                        (value no)
                        (certainty 30.0))
            )  
)

(defrule SCEGLI-QUALITA::best-fumatore-no
            (attribute (nome fuma) (value ?value & no))
      =>
            (assert (attribute (nome best-balcone) 
                        (value si)
                        (certainty 50.0))
            )
            (assert (attribute (nome best-balcone) 
                        (value no)
                        (certainty 50.0))
            )  
)

; Scelta pendolare

(defrule SCEGLI-QUALITA::best-pendolare-si
            (attribute (nome fa-pendolare) (value ?value & pendolare))
            (quartiere (nome ?nq) (servizi $? stazione $?))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 80.0))
            )
)

(defrule SCEGLI-QUALITA::best-pendolare-torino
            (attribute (nome fa-pendolare) (value ?value & torino))
            (or (quartiere (nome ?nq) (servizi $? bus $?))
                (quartiere (nome ?nq) (servizi $? metro $?))
            )
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 80.0))
            )
)

(defrule SCEGLI-QUALITA::best-pendolare-macchina
            (attribute (nome fa-pendolare) (value ?value & macchina))
            (quartiere (nome ?nq))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 70.0))
            ) 
)

; Scelta aperitivo

(defrule SCEGLI-QUALITA::best-aperitivo-si
            (attribute (nome fa-aperitivi) (value ?value & si))
      =>
            (assert (attribute (nome best-terrazzino) 
                        (value si)
                        (certainty 80.0))
            )
            (assert (attribute (nome best-terrazzino) 
                        (value no)
                        (certainty 60.0))
            )  
)

(defrule SCEGLI-QUALITA::best-aperitivo-no
            (attribute (nome fa-aperitivi) (value ?value & no))
      =>
            (assert (attribute (nome best-terrazzino) 
                        (value no)
                        (certainty 60.0))
            )
            (assert (attribute (nome best-terrazzino) 
                        (value si)
                        (certainty 80.0))
            )  
)

(defrule SCEGLI-QUALITA::best-aperitivo-ogni-tanto
            (attribute (nome fa-aperitivi) (value ?value & ogni-tanto))
      =>
            (assert (attribute (nome best-terrazzino) 
                        (value no)
                        (certainty 60.0))
            )
            (assert (attribute (nome best-terrazzino) 
                        (value si)
                        (certainty 60.0))
            )  
)

; Scelta cane

(defrule SCEGLI-QUALITA::best-cane-si
            (attribute (nome ha-cane) (value ?value & si))
            (not (attribute (nome casa-indipendente) (value ?value2 & si | no)))
      =>
            (assert (attribute (nome best-indipendente) 
                        (value si)
                        (certainty 80.0))
            )
            (assert (attribute (nome best-indipendente) 
                        (value no)
                        (certainty 30.0))
            )
)

(defrule SCEGLI-QUALITA::best-cane-si-quart
            (attribute (nome ha-cane) (value ?value & si))
            (quartiere (nome ?nq) (servizi $? parco $?))
      =>
            (assert (attribute (nome best-quartiere) 
                        (value ?nq)
                        (certainty 70.0))
            )
)

;;*************************
;;* MODIFICA PREFERENZE   *
;;*************************

(defmodule MODIFICA-PREFERENZE (import MAIN ?ALL))

; Modifiche preferenze zona

(defrule MODIFICA-PREFERENZE::modifica-zona-cancella
      (declare (salience 20))
           (or    (attribute (nome modifiche) (value ?value & zona-centro | zona-periferia | zona-prima-cintura))
                  (attribute (nome modifiche-casa-zona) (value ?value & ~qualsiasi))
           )
      ?a <- (attribute (nome best-zona) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-zona-inserisci
      (declare (salience 10))
       (or  ?a <-  (attribute (nome modifiche) (value ?value & zona-centro | zona-periferia | zona-prima-cintura))
            ?a <-  (attribute (nome modifiche-casa-zona) (value ?value & centro | periferia | prima-cintura))
       )
      =>
            (if (or (eq ?value zona-centro) (eq ?value centro))
             then (assert (attribute (nome best-zona) 
                     (value centro)))
            )
            (if (or (eq ?value zona-periferia) (eq ?value periferia))
             then (assert (attribute (nome best-zona) 
                     (value periferia)))
            )
            (if (or (eq ?value zona-prima-cintura) (eq ?value prima-cintura))
             then (assert (attribute (nome best-zona) 
                     (value prima-cintura)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-zona-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-casa-zona) (value ?value & preferisco-centro | preferisco-periferia | preferisco-prima-cintura))
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
            (retract ?a)
)

; Modifiche preferenze metri quadri

(defrule MODIFICA-PREFERENZE::modifica-metri-quadri-cancella
      (declare (salience 20))
            (attribute (nome modifiche) (value ?value & metri-30 | metri-40 | metri-50 | metri-60 | metri-70 | metri-80 | metri-90 | metri-100 | metri-120 | metri-140 | metri-160 | metri-180 | metri-200 | metri-250 | metri-300 | metri-400 | metri-500))
      ?a <- (attribute (nome best-metriquadri) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-metri-quadri-inserisci
      (declare (salience 10))
      ?a <- (attribute (nome modifiche) (value ?value & metri-30 | metri-40 | metri-50 | metri-60 | metri-70 | metri-80 | metri-90 | metri-100 | metri-120 | metri-140 | metri-160 | metri-180 | metri-200 | metri-250 | metri-300 | metri-400 | metri-500))
      =>
            (if (eq ?value metri-30)
             then (assert (attribute (nome best-metriquadri) 
                     (value 30)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 40)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 20)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-40)
             then (assert (attribute (nome best-metriquadri) 
                     (value 40)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 50)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 30)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-50)
             then (assert (attribute (nome best-metriquadri) 
                     (value 50)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 60)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 40)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-60)
             then (assert (attribute (nome best-metriquadri) 
                     (value 60)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 70)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 50)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-70)
             then (assert (attribute (nome best-metriquadri) 
                     (value 70)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 80)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 60)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-80)
             then (assert (attribute (nome best-metriquadri) 
                     (value 80)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 90)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 70)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-90)
             then (assert (attribute (nome best-metriquadri) 
                     (value 90)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 100)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 80)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-100)
             then (assert (attribute (nome best-metriquadri) 
                     (value 100)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 110)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 90)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-120)
             then (assert (attribute (nome best-metriquadri) 
                     (value 120)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 130)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 110)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-140)
             then (assert (attribute (nome best-metriquadri) 
                     (value 140)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 150)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 130)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-160)
             then (assert (attribute (nome best-metriquadri) 
                     (value 160)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 170)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 150)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-180)
             then (assert (attribute (nome best-metriquadri) 
                     (value 180)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 190)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 170)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-200)
             then (assert (attribute (nome best-metriquadri) 
                     (value 200)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 210)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 190)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-250)
             then (assert (attribute (nome best-metriquadri) 
                     (value 250)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 260)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 240)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-300)
             then (assert (attribute (nome best-metriquadri) 
                     (value 300)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 310)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 290)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-400)
             then (assert (attribute (nome best-metriquadri) 
                     (value 400)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 410)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 390)
                     (certainty 70.0)))
            )
            (if (eq ?value metri-500)
             then (assert (attribute (nome best-metriquadri) 
                     (value 500)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 510)
                     (certainty 70.0)))
                  (assert (attribute (nome best-metriquadri) 
                     (value 490)
                     (certainty 70.0)))
            )
            (retract ?a)
)

; Modifiche preferenza indipendente

(defrule MODIFICA-PREFERENZE::modifica-indipendente-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & indipendente-si | indipendente-no))
            (attribute (nome modifiche-casa-indipendente) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-indipendente) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-indipendente-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & indipendente-si | indipendente-no))
            ?a <- (attribute (nome modifiche-casa-indipendente) (value ?value & si | no))
      )
      =>
            (if (or (eq ?value indipendente-si) (eq ?value si))
             then (assert (attribute (nome best-indipendente) 
                     (value si)))
             else (assert (attribute (nome best-indipendente) 
                     (value no)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-indipendente-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-casa-indipendente) (value ?value & preferisco-si | preferisco-no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-indipendente) 
                     (value si)
                     (certainty 80.0)))
                  (assert (attribute (nome best-indipendente) 
                     (value no)
                     (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-indipendente) 
                     (value no)
                     (certainty 80.0)))
                  (assert (attribute (nome best-indipendente) 
                     (value si)
                     (certainty 20.0)))
            )
            (retract ?a)
)

; Modifiche preferenza indipendente

(defrule MODIFICA-PREFERENZE::modifica-piano-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & piano-alto | piano-basso | piano-terra))
            (attribute (nome modifiche-casa-piano) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-piano) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-piano-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & piano-alto | piano-basso | piano-terra))
            ?a <- (attribute (nome modifiche-casa-piano) (value ?value & alto | basso | terra))
      )
      =>
            (if (or (eq ?value piano-alto) (eq ?value alto))
             then (assert (attribute (nome best-piano) 
                     (value alto)))
            )
            (if (or (eq ?value piano-basso) (eq ?value basso))
             then (assert (attribute (nome best-piano) 
                     (value basso)))
            )
            (if (or (eq ?value piano-terra) (eq ?value terra))
             then (assert (attribute (nome best-piano) 
                     (value terra)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-piano-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-casa-piano) (value ?value & preferisco-alto | preferisco-basso | preferisco-terra))
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
            )
            (retract ?a)
)

; Modifiche preferenza ascensore

(defrule MODIFICA-PREFERENZE::modifica-ascensore-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & ascensore-si | ascensore-no))
            (attribute (nome modifiche-ha-ascensore) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-ascensore) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-ascensore-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & ascensore-si | ascensore-no))
            ?a <- (attribute (nome modifiche-ha-ascensore) (value ?value & si | no))
      )
      =>
            (if (or (eq ?value ascensore-si) (eq ?value si))
             then (assert (attribute (nome best-ascensore) 
                     (value si)))
             else (assert (attribute (nome best-ascensore) 
                     (value no)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-ascensore-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-ha-ascensore) (value ?value & preferisco-si | preferisco-no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-ascensore) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-ascensore) 
                          (value si)
                          (certainty 30.0)))
            )
            (retract ?a)
)

; Modifiche preferenza bagni

(defrule MODIFICA-PREFERENZE::modifica-bagni-cancella
      (declare (salience 20))
            (attribute (nome modifiche) (value ?value & bagni-1 | bagni-2 | bagni-3 | bagni-4))
      ?a <- (attribute (nome best-bagni) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-bagni-inserisci
      (declare (salience 10))
      ?a <- (attribute (nome modifiche) (value ?value & bagni-1 | bagni-2 | bagni-3 | bagni-4))
      =>
            (if (eq ?value bagni-1)
             then (assert (attribute (nome best-bagni) 
                     (value 1)))
                  (assert (attribute (nome best-bagni) 
                     (value 2)
                     (certainty 90.0)))
            )
            (if (eq ?value bagni-2)
             then (assert (attribute (nome best-bagni) 
                     (value 2)))
                  (assert (attribute (nome best-bagni) 
                     (value 3)
                     (certainty 90.0)))
                  (assert (attribute (nome best-bagni) 
                     (value 1)
                     (certainty 60.0)))
            )
            (if (eq ?value bagni-3)
             then (assert (attribute (nome best-bagni) 
                     (value 3)))
                  (assert (attribute (nome best-bagni) 
                     (value 4)
                     (certainty 90.0)))
                  (assert (attribute (nome best-bagni) 
                     (value 2)
                     (certainty 60.0)))
            )
            (if (eq ?value bagni-4)
             then (assert (attribute (nome best-bagni) 
                     (value 4)))
                  (assert (attribute (nome best-bagni) 
                     (value 3)
                     (certainty 60.0)))
            )
            (retract ?a)
)

; Modifiche preferenza balcone

(defrule MODIFICA-PREFERENZE::modifica-balcone-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & balcone-si | balcone-no))
            (attribute (nome modifiche-ha-balcone) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-balcone) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-balcone-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & balcone-si | balcone-no))
            ?a <- (attribute (nome modifiche-ha-balcone) (value ?value & si | no))
      )
      =>
            (if (or (eq ?value balcone-si) (eq ?value si))
             then (assert (attribute (nome best-balcone) 
                     (value si)))
             else (assert (attribute (nome best-balcone) 
                     (value no)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-balcone-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-ha-balcone) (value ?value & preferisco-si | preferisco-no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-balcone) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-balcone) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-balcone) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-balcone) 
                          (value si)
                          (certainty 30.0)))
            )
            (retract ?a)
)

; Modifiche preferenza boxauto

(defrule MODIFICA-PREFERENZE::modifica-boxauto-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & boxauto-si | boxauto-no))
            (attribute (nome modifiche-ha-box) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-boxauto) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-boxauto-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & boxauto-si | boxauto-no))
            ?a <- (attribute (nome modifiche-ha-box) (value ?value & si | no))
      )
      =>
            (if (or (eq ?value boxauto-si) (eq ?value si))
             then (assert (attribute (nome best-boxauto) 
                     (value si)))
             else (assert (attribute (nome best-boxauto) 
                     (value no)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-boxauto-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-ha-box) (value ?value & preferisco-si | preferisco-no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-boxauto) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-boxauto) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-boxauto) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-boxauto) 
                          (value si)
                          (certainty 30.0)))
            )
            (retract ?a)
)

; Modifiche preferenza garage

(defrule MODIFICA-PREFERENZE::modifica-garage-cancella
      (declare (salience 20))
      (or   (attribute (nome modifiche) (value ?value & garage-si | garage-no))
            (attribute (nome modifiche-ha-garage) (value ?value & ~qualsiasi))
      )
      ?a <- (attribute (nome best-garage) (value ?value2 & ~any))
      =>
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-garage-inserisci
      (declare (salience 10))
      (or   ?a <- (attribute (nome modifiche) (value ?value & garage-si | garage-no))
            ?a <- (attribute (nome modifiche-ha-garage) (value ?value & si | no))
      )
      =>
            (if (or (eq ?value garage-si) (eq ?value si))
             then (assert (attribute (nome best-garage) 
                     (value si)))
             else (assert (attribute (nome best-garage) 
                     (value no)))
            )
            (retract ?a)
)

(defrule MODIFICA-PREFERENZE::modifica-garage-inserisci-preferisco
      (declare (salience 10))
      ?a <- (attribute (nome modifiche-ha-garage) (value ?value & preferisco-si | preferisco-no))
      =>
            (if (eq ?value preferisco-si)
             then (assert (attribute (nome best-garage) 
                          (value si)
                          (certainty 80.0)))
                  (assert (attribute (nome best-garage) 
                          (value no)
                          (certainty 30.0)))
            )
            (if (eq ?value preferisco-no)
             then (assert (attribute (nome best-garage) 
                          (value no)
                          (certainty 80.0)))
                  (assert (attribute (nome best-garage) 
                          (value si)
                          (certainty 30.0)))
            )
            (retract ?a)
)

;;*************************
;;* REGOLE SELEZIONE CASE *
;;*************************

(defmodule CASE (import MAIN ?ALL)
                (import QUARTIERI ?ALL)
                (export ?ALL)
)

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
        (ascensore no) (boxauto si 15) (garage no) (terrazzino no) (balcone si) (indipendente si) (bagni 1))
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
  (casa (nome casaCrocetta2) (metriquadri 70) (vani 2) (piano basso 1) (citta torino) (zona prima-cintura) (quartiere crocetta) 
        (garage si) (boxauto no) (balcone no) (terrazzino no) (indipendente no) (bagni 1))
)
  
(defrule CASE::genera-case
  (attribute (nome prezzo-massimo) (value ?prezzoMax))
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
        (prezzo ?prezzo &:(<= (integer ?prezzo) (integer ?prezzoMax)))
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
  (not (attribute (nome best-quartiere) (value ?quart) (certainty ?certainty-19)))
  =>
  (assert (attribute (nome casa) (value ?nome)
                (certainty (min ?certainty-1 ?certainty-16 ?certainty-4
                                ?certainty-6 ?certainty-15 ?certainty-17
                                ?certainty-9  ?certainty-14 ?certainty-18
                          ))))
  )

(defrule CASE::genera-case2
  (attribute (nome prezzo-massimo) (value ?prezzoMax))
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
        (prezzo ?prezzo &:(<= (integer ?prezzo) (integer ?prezzoMax)))
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
  (attribute (nome best-quartiere) (value ?quart) (certainty ?certainty-19))
  =>
  (assert (attribute (nome casa) (value ?nome)
                (certainty (min ?certainty-1 ?certainty-16 ?certainty-4
                                ?certainty-6 ?certainty-15 ?certainty-17
                                ?certainty-9  ?certainty-14 ?certainty-18 ?certainty-19
                          ))))
  )

;;*********************************
;;* REGOLE MODIFICHE DOMANDE REDO *
;;*********************************

(defmodule GENERA-PREZZI    (import MAIN ?ALL)
                            (import QUARTIERI ?ALL)
                            (import CASE ?ALL)
)

(defrule GENERA-PREZZI::genera-prezzo
   ?f <- (casa (prezzo any) (metriquadri ?mq) (quartiere ?q) (boxauto $? ?b ?bmq $?) (garage ?g))
         (quartiere (nome ?q) (costo-mq ?cmq) (servizi $?s))
   =>    (modify ?f (prezzo (+ (+ (+ (* ?cmq ?mq) 30000) (* (length$ $?s) 5000)) (* ?bmq ?cmq))))
         (modify ?f (servizi (length$ $?s)))         
)

(defrule GENERA-PREZZI::genera-prezzo-any
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

;;*****************************
;;* STAMPA RISULTATI CASE     *
;;*****************************

(defmodule PRINT-RESULTS (import MAIN ?ALL))

(defrule PRINT-RESULTS::header ""
   (declare (salience 10))
   =>
   (format t "%n------------------------------- CASE SELEZIONATE  --->  CASA  CERTAINTY -------------------------------%n")
)

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
   (format t "-------------------------------------------------------------------------------------------------------")
   (format t "%n%n")
)