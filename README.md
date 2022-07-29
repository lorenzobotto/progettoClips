# progettoClips
Progetto CLIPS per il corso di Intelligenza Artificiale e Laboratorio - Unito 2022.

## Descrizione progetto

L’obiettivo del progetto è quello di sviluppare un sistema esperto che supporti un agente
immobiliare nel suo lavoro: suggerire a un potenziale cliente un numero ristretto di alloggi in
vendita da visitare.

Ogni casa è modellata secondo diverse proprietà (es: metri quadri, numero dei bagni, zona, ecc...).

Per la realizzazione del sistema intelligente ho definito vari moduli e creato un ciclo di esecuzioni che si compone di queste fasi:
- viene effettuato un primo giro di domande per capire a cosa è interessato l’utente;
- verranno stampati i risultati con il loro CF[^1];
- viene fatto un secondo giro di domande più personali all’utente;
- verranno stampati i risultati con il loro CF;
- viene fatto un terzo giro di domande dove l’utente può dare una risposta più precisa ad una domanda dove era incerto precedentemente oppure modificare qualsiasi preferenza data in precedenza.

Al termine di ciò si vedranno le case con i loro CF a seconda di cosa l’utente vorrebbe e quali case sono a disposizione.
Per la realizzazione ho inserito un database di case e quartieri inerenti solo alla città di Torino, ma è funzionante anche se si inseriscono altre città e quartieri.

Ad ogni giro di domande vengono inseriti vincoli hard e soft per selezionare delle case piuttosto che altre, con il loro CF, sulla base delle risposte dell'utente.

[^1]: Certainty Factor: E' il fattore di certezza. Viene assegnato un valore CF alle case per indicare la certezza con cui quella casa verrà scelta dall'utente.

**Disclaimer: Tutti i dati inseriti da me sono dati di prova, completamente casuali e non sono da intendere come reali. Derivano tutti dalla mia immaginazione e inseriti casualmente. Non utilizzare alcun dato per nessun scopo particolare. Non mi assumo nessuna responsabilità.**

## Come eseguire

Installare CLIPS da https://www.clipsrules.net/. Una volta installato, utilizzare l'IDE per eseguire.

Per eseguire il codice caricare il file su CLIPS tramite il comando (oppure da interfaccia senza il comando ma tramite menù 'Environment --> Load Constructs'):
```
(load "case.clp")
```

Eseguire un reset:
```
(reset)
```

Ed infine eseguire:
```
(run)
```

## Video esecuzione del progetto

<a href="http://www.youtube.com/watch?feature=player_embedded&v=j-l6Q-yiKls
" rel="noopener" target="_blank"><img src="http://img.youtube.com/vi/j-l6Q-yiKls/0.jpg" 
alt="Esecuzione CLIPS" width="400" border="10" /></a>
