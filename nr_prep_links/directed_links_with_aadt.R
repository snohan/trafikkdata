# Create a geopackage-file containing directed links with aadt

# Framgangsmåte for å lage vegnettsgraf, steg 1 i ÅDT-modulen
#
# 1. Først isoleres hovedkomponenten i vegnettet gjennom en nettverksanalyse på
#    samlingen av trafikklenker og kryss (NR-kode).
#
# 2. Deretter retningsdekomponeres lenkene i hovedkomponenten (TØI-kode). Dette
#    gir en sammenhengende vegnettsgraf med retningsoppløsning.
#
# 3. Fra vegnettsgrafen beregnes sentralitetsmålet betweenness centrality til bruk i den
#    statistiske modellen (NR-kode). Sentralitetsverdier tilordnes hver enkelt lenke i
#    den rettede grafen.
#
# 4. Preliminær ÅDT fra tilgjengelige trafikkregistreringspunkter (kontinuerlige og
#    periodiske) kobles på sine respektive rettede lenker der hvor slike punkter finnes
#    (Knowit-kode).
#
# 5. For å få med nettutlagt ÅDT fra transportmodellkjøringen for 2018, må det dessuten
#    opprettes en kobling mellom den nye vegnettsgrafen og lenkene i vegnettsgrafen
#    fra det inneværende prosjektet (ny kode).
#
# 6. Den nye vegnettsgrafen med tilhørende attributter sammenstilles til en GeoPackagefil.


# Problemer
#
# 1. TØI laget en liste med lenker som skulle fjernes. ID-ene er endret, så disse kan ikke
#    fjernes slik de er angitt nå. Er dette fortsatt nødvendig? Finnes det andre lenker som burde plukkes vekk?
#
# 2. Grafen er veldig usammenhengende med sine over 2 000 komponenter.
#    Vi burde sjekke alle løse vegnettsdeler - skulle de vært en del av hovedkomponenten?


# Isolate main component and make directed links ----
