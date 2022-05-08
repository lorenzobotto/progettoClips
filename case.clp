;;*************************
;;* REGOLE SELEZIONE CASE *
;;*************************

(defmodule CASE (import MAIN ?ALL))

(deffacts any-attributes
  (attribute (name best-metriquadri) (value any))
  (attribute (name best-vani) (value any))
  (attribute (name best-servizi) (value any))
  (attribute (name best-piano) (value any))
  (attribute (name best-citta) (value any))
  (attribute (name best-zona) (value any))
  (attribute (name best-quartiere) (value any))
  (attribute (name best-ascensore) (value any))
  (attribute (name best-boxauto) (value any))
  (attribute (name best-terrazzino) (value any))
  (attribute (name best-balcone) (value any))
  (attribute (name best-prezzo) (value any))
  (attribute (name best-indipendente) (value any))
  (attribute (name best-bagni) (value any))
)

(deftemplate CASE::casa
  (slot nome (type STRING) (default any))
  (slot metriquadri (type INTEGER) (default any))
  (slot vani (type INTEGER) (default any))
  (slot servizi (type INTEGER) (default any))
  (slot piano (type INTEGER) (default any))
  (slot citta (type STRING) (default any))
  (slot zona (type STRING) (default any))
  (slot quartiere (type STRING) (default any))
  (slot ascensore (type SYMBOL) (allowed-symbols si no))
  (slot boxauto (type SYMBOL) (allowed-symbols si no))
  (slot garage (type SYMBOL) (allowed-symbols si no))
  (slot terrazzino (type SYMBOL) (allowed-symbols si no))
  (slot balcone (type SYMBOL) (allowed-symbols si no))
  (slot prezzo (type INTEGER) (default any))
  (slot indipendente (type SYMBOL) (allowed-symbols si no))
  (slot bagni (type INTEGER) (default any))
)

(deffacts CASE::casa-lista 
  (casa (nome casaCentro) (metriquadri 55) (vani 2) (servizi 5) (piano 1) (citta torino) (zona centro) (quartiere quadrilateroRomano) 
        (ascensore no) (boxauto no) (garage no) (terrazzino no) (balcone si) (prezzo 300000) (indipendente no) (bagni 1))
  (casa (nome casaPeriferia) (metriquadri 58) (vani 2) (servizi 3) (piano 6) (citta torino) (zona periferia) (quartiere pozzoStrada) 
        (ascensore si) (boxauto si) (garage no) (terrazzino si) (balcone si) (prezzo 200000) (indipendente no) (bagni 1))
  (casa (nome villa) (metriquadri 250) (vani 7) (servizi 4) (piano 0) (citta torino) (zona centro) (quartiere colleMaddalena) 
        (ascensore no) (boxauto no) (garage si) (terrazzino si) (balcone si) (prezzo 800000) (indipendente si) (bagni 3))
)
  
(defrule CASE::genera-case
  (casa (nome ?nome)
        (metriquadri ?mq)
        (vani $? ?vani $?)
        (servizi $? ?serv $?)
        (piano $? ?piano $?)
        (citta ?citta)
        (zona $? ?zona $?)
        (quartiere $? ?quart $?)
        (ascensore $? ?asce $?)
        (boxauto ?box)
        (garage $? ?garage $?)
        (terrazzino $? ?terraz $?)
        (balcone $? ?balcone $?)
        (prezzo $? ?prezzo $?)
        (indipendente $? ?indip $?)
        (bagni $? ?bagni $?)
  )
  (attribute (name best-metriquadri) (value ?mq) (certainty ?certainty-1))
  (attribute (name best-vani) (value ?vani) (certainty ?certainty-2))
  (attribute (name best-servizi) (value ?serv) (certainty ?certainty-3))
  (attribute (name best-piano) (value ?piano) (certainty ?certainty-4))
  (attribute (name best-citta) (value ?citta) (certainty ?certainty-5))
  (attribute (name best-zona) (value ?zona) (certainty ?certainty-6))
  (attribute (name best-quartiere) (value ?quart) (certainty ?certainty-7))
  (attribute (name best-ascensore) (value ?asce) (certainty ?certainty-8))
  (attribute (name best-boxauto) (value ?box) (certainty ?certainty-9))
  (attribute (name best-garage) (value ?garage) (certainty ?certainty-10))
  (attribute (name best-terrazzino) (value ?terraz) (certainty ?certainty-11))
  (attribute (name best-balcone) (value ?balcone) (certainty ?certainty-12))
  (attribute (name best-prezzo) (value ?prezzo) (certainty ?certainty-13))
  (attribute (name best-indipendente) (value ?indip) (certainty ?certainty-14))
  (attribute (name best-bagni) (value ?bagni) (certainty ?certainty-15))
  =>
  (assert (attribute (name casa) (value ?nome)
                     (certainty (min ?certainty-1 ?certainty-2 ?certainty-3 ?certainty-4
                                     ?certainty-5 ?certainty-6 ?certainty-7 ?certainty-8
                                     ?certainty-9 ?certainty-10 ?certainty-11 ?certainty-12
                                     ?certainty-13 ?certainty-14 ?certainty-15
                                )))))