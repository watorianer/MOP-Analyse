km.max = 350
km.akt = 17.5
km.sof = 54.5
km.spä = 17.5
lad.max = 35
t.sta = 2.5

ant.sof = min((km.sof - km.akt)/lad.max, t.sta)
ant.spä = max((km.spä - km.sof)/lad.max, 0)

t.lad = min(t.sta, (km.max - km.akt)/lad.max)

lad.spä = max((km.spä - km.sof), 0)/(t.sta - ant.sof)
lad.ant = lad.spä/lad.max
  

cf = 1 - ((ant.sof + (lad.ant)*ant.spä)/t.lad)

if (ant.sof == t.sta) {
  cat("Keine Ladeflexibilität freigegeben, da mindestens die gesamte Standdauer benötigt wird um die geforderte Mindestreichweite sofort erreichen zu können.")
} else if (lad.ant >= 1) {
  cat("Keine Ladeflexibilität freigegeben, da mindestens mit der maximalen Aufladekapazität geladen werden muss um die geforderte Mindestreichweite später erreichen zu können.")
} else {
  cat("Die freigegebene Ladeflexibilität beträgt", cf*100, "%")
}