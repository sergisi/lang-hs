# Anàlisi semàntica i generació de codi intermedi

## Estructura general

Carpeta app:
* **Lexer.x:** És el fitxer on definim l'analitzador lèxic.
* **Parser.y:** És el fitxer on definim l'analitzador sintàctic
* **ParserData.hs:** És el fitxer amb les definicions de tipus que utilitzarem en la part de l'anàlisi sintàctic. 
* **SyntaxAnalysis:** És el fitxer amb totes les funcions auxiliars que utilitzarem per l'anàlisi semantic i generació de codi. Ho hem decidit fer així perque si no el parser contenia massa lògica i feia més dificil la seva lectura i compensió.
* **AlexUserState:** Es defineix un estat dins la monada Alex que és el que mantindrà la taula de tipus.

Carpeta test amb els testos de cada un dels exercicis, s'han realitzat amb la llibreria Tasty.

## Requeriments Tècnics:

### Alex
* L’analitzador lèxic. Ve juntament amb la plataforma de haskell. En el desenvolupament, s’ha provat amb les versions 3.2.4.

### Happy
* L'analitzador sintàctic emprat. També ve conjuntament amb la plataforma de Haskell. S'ha probat amb la versió 1.20.0

### GHC
* Compilador de haskell. S’ha utilitzat la versió 8.10.4.

### Cabal
* Gestor de paquets de haskell. Ha sofert canvis dràstics en l’últim any, pel que es necessari una versió superior com a mínim a 3.4.0. Per a actualitzar el cabal donat un cabal instal·lat, utilitza la comanda cabal new-install Cabal cabal-install. 

## Execució:
Per tal d'executar els exercicis, moures a la carpeta  de l'exercici i executar:
```
make all
cabal v2-run code
```
Per tal d'executar els tests:
```
cabal v2-run test-suite:TestSuite
```

## Documentació
La documentació es troba a la carpeta doc sota el nom docu.pdf.

## Link Repo
https://github.com/sergisi/lang-hs

## Autors
Sergi Simón Balcells
Joaquim Picó Mora