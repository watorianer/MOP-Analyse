km.max = 350
km.akt = 17.5
km.sof = 54.5
km.sp� = 17.5
lad.max = 35
t.sta = 2.5

ant.sof = min((km.sof - km.akt)/lad.max, t.sta)
ant.sp� = max((km.sp� - km.sof)/lad.max, 0)

t.lad = min(t.sta, (km.max - km.akt)/lad.max)

lad.sp� = max((km.sp� - km.sof), 0)/(t.sta - ant.sof)
lad.ant = lad.sp�/lad.max
  

cf = 1 - ((ant.sof + (lad.ant)*ant.sp�)/t.lad)

if (ant.sof == t.sta) {
  cat("Keine Ladeflexibilit�t freigegeben, da mindestens die gesamte Standdauer ben�tigt wird um die geforderte Mindestreichweite sofort erreichen zu k�nnen.")
} else if (lad.ant >= 1) {
  cat("Keine Ladeflexibilit�t freigegeben, da mindestens mit der maximalen Aufladekapazit�t geladen werden muss um die geforderte Mindestreichweite sp�ter erreichen zu k�nnen.")
} else {
  cat("Die freigegebene Ladeflexibilit�t betr�gt", cf*100, "%")
}