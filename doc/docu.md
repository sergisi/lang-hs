# Pràctica III: Anàlisi semàntica i generació de codi intermedi

Alumnes: Joaquim Picó Mora, Sergi Simón Balcells

Professora: Maria Teresa Alsinet Bernadó

Curs: 2020-2021

## Caràcteristiques bàsiques
A continuació és mostra un llistat de les caràcteristiques principals que s'han implementat en aquest llenguatge:
- Tots els valors són constants
- S'ha implementat la definició de funcions (Veure a test/files/functions)
- S'ha implementat la crida de funcions
- Adicionalment, s'ha implementat la currificació dels paràmetres de les funcions
- No hi ha constants globals, només funcions globals. Tot és defineix dins una fuinció.
- Àmbits de programa: S'ha pensat com una llista(pila) que resideix en la mònada AlexState en la qual es van afegint Maps a mesura que es creen contextos diferents.
- Soporta els tipus bolea, enter, caràcter i real.
- Es poden crear arrays i arrays n dimensionals (Veure a test/files/arrays)
- S'han creat totes les expressions d'enters, dels reals, dels boleans i algunes per el tipus caràcter. (Veure a test/files/expresions)
- S'ha creat l'estructura d'assignació per a les funcions i per a data.
- S'ha creat l'estructura if/else (veure a test/files/conditional)
- S'ha creat l'estructura while (veure a test/files/while) 
- S'ha creat l'estructura for (veure a test/files/for)  
- S'ha creat l'estructura repeat/until (veure a test/files/repeat_until)  
- Adicionalment s'ha creat l'estructura map (veure a test/files/map)  
- Adicionalment s'ha implementat la definició de tipus mitjançant data. (Veure a test/files/data)
- Juntament amb data s'ha implementat l'estructura case of.

## Three Address Code
Es pot veure la sintàxis del còdi generat en els fitxers amb extensió .tac de la carpeta test/files. És la carpeta on hi ha tots els testos, en els casos en que testejem errors no es jenera el codi de tres adreces si no que es printa l'error. La sintàxi es pot observar en els fitxers .tac que es generen a partir dels testos amb el còdi correcte.

## Taula de simbols
En la mònada d'estat que ens dona Àlex i emmagatzemem dos Maps. El map de definicions on s'hi guardaran les definicions de tipus que realitzem mitjançant data. I el Map de valors, on s'hi guardara tota la resta de definicions.