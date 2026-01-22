packages <- c("tidyverse", "knitr", "rmarkdown","roxygen2","testthat","usethis","devtools","ggplot2","ggrepel","stats","kableExtra","bookdown","shiny","patchwork","dplyr","broom")
lapply(packages, library, character.only=TRUE)

tuesdata <- tidytuesdayR::tt_load('2021-09-14')
audio_features <- tuesdata$audio_features

AA <- audio_features %>% select(danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,time_signature,spotify_track_popularity) %>% filter(is.na(spotify_track_popularity)==0 & is.na(danceability)==0)

a <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=danceability))+ geom_point() + stat_smooth(method="lm", col="red")
b <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=energy))+ geom_point() + stat_smooth(method="lm", col="red")
c <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=key))+ geom_point() + stat_smooth(method="lm", col="red")
d <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=loudness))+ geom_point() + stat_smooth(method="lm", col="red")
e <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=mode))+ geom_point() + stat_smooth(method="lm", col="red")
f <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=speechiness))+ geom_point() + stat_smooth(method="lm", col="red")
g <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=acousticness))+ geom_point() + stat_smooth(method="lm", col="red")
h <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=instrumentalness))+ geom_point() + stat_smooth(method="lm", col="red")
i <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=liveness))+ geom_point() + stat_smooth(method="lm", col="red")
j <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=valence))+ geom_point() + stat_smooth(method="lm", col="red")
k <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=tempo))+ geom_point() + stat_smooth(method="lm", col="red")
l <- ggplot2::ggplot(AA,aes(x=spotify_track_popularity,y=time_signature))+ geom_point() + stat_smooth(method="lm", col="red")

print((a+b)/(c+d)/(e+f)/(g+h)/(i+j)/(k+l))

aa <- lm(danceability~spotify_track_popularity,AA)
bb <- lm(energy~spotify_track_popularity,AA)
cc <- lm(key~spotify_track_popularity,AA)
dd <- lm(loudness~spotify_track_popularity,AA)
ee <- lm(mode~spotify_track_popularity,AA)
ff <- lm(speechiness~spotify_track_popularity,AA)
gg <- lm(acousticness~spotify_track_popularity,AA)
hh <- lm(instrumentalness~spotify_track_popularity,AA)
ii <- lm(liveness~spotify_track_popularity,AA)
jj <- lm(valence~spotify_track_popularity,AA)
kk <- lm(tempo~spotify_track_popularity,AA)
ll <- lm(time_signature~spotify_track_popularity,AA)

aas <- glance(aa)
bbs <- glance(bb)
ccs <- glance(cc)
dds <- glance(dd)
ees <- glance(ee)
ffs <- glance(ff)
ggs <- glance(gg)
hhs <- glance(hh)
iis <- glance(ii)
jjs <- glance(jj)
kks <- glance(kk)
lls <- glance(ll)

aas$p.value
bbs$p.value
ccs$p.value
dds$p.value
ees$p.value
ffs$p.value
ggs$p.value
hhs$p.value
iis$p.value
jjs$p.value
kks$p.value
lls$p.value

a2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=aa$fitted.values ,y=aa$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
b2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=bb$fitted.values ,y=bb$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
c2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=cc$fitted.values ,y=cc$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
d2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=dd$fitted.values ,y=dd$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
e2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=ee$fitted.values ,y=ee$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
f2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=ff$fitted.values ,y=ff$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
g2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=gg$fitted.values ,y=gg$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
h2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=hh$fitted.values ,y=hh$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
i2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=ii$fitted.values ,y=ii$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
j2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=jj$fitted.values ,y=jj$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
k2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=kk$fitted.values ,y=kk$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")
l2 <- ggplot2::ggplot(AA)+geom_point(mapping=aes(x=ll$fitted.values ,y=ll$residuals)) + geom_hline(yintercept=0,lwd=2)+ylab("Residuals")+xlab("Fitted values")

print((a2+b2)/(c2+d2)/(e2+f2)/(g2+h2)/(i2+j2)/(k2+l2))

a3 <- ggplot2::ggplot(AA, aes(sample=danceability))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
b3 <- ggplot2::ggplot(AA, aes(sample=energy))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
c3 <- ggplot2::ggplot(AA, aes(sample=key))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
d3 <- ggplot2::ggplot(AA, aes(sample=loudness))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
e3 <- ggplot2::ggplot(AA, aes(sample=mode))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
f3 <- ggplot2::ggplot(AA, aes(sample=speechiness))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
g3 <- ggplot2::ggplot(AA, aes(sample=acousticness))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
h3 <- ggplot2::ggplot(AA, aes(sample=instrumentalness))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
i3 <- ggplot2::ggplot(AA, aes(sample=liveness))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
j3 <- ggplot2::ggplot(AA, aes(sample=valence))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
k3 <- ggplot2::ggplot(AA, aes(sample=tempo))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")
l3 <- ggplot2::ggplot(AA, aes(sample=time_signature))+geom_qq()+geom_qq_line()+xlab("theoretical")+ylab("sample")

print((a3+b3)/(c3+d3)/(e3+f3)/(g3+h3)/(i3+j3)/(k3+l3))
