\name{pems.object}
\Rdversion{1.1}
\alias{pems.object}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(pems.object)}
\format{
  The format is:
List of 7
 $ data     :'data.frame':	3341 obs. of  25 variables:
  ..$ time.stamp   : POSIXct[1:3341], format: "2007-03-01 16:27:36" "2007-03-01 16:27:37" ...
  ..$ local.time   : num [1:3341] 0 1 2 3 4 5 6 7 8 9 ...
  ..$ conc.co      : num [1:3341] 0.000549 0.000183 0.000916 0 0.000549 ...
  ..$ conc.co2     : num [1:3341] 0 0.0513 0.0671 0.0586 0.061 ...
  ..$ conc.hc      : num [1:3341] 0 2.75 5.04 5.8 5.19 ...
  ..$ conc.nox     : num [1:3341] 15.9 18.3 15.9 18.3 19.5 ...
  ..$ afr          : num [1:3341] 200 200 200 200 200 ...
  ..$ exh.flow.rate: num [1:3341] -46.86 -2.19 20.73 -15.76 -2.65 ...
  ..$ exh.temp     : num [1:3341] 32.7 31.8 30.9 31.1 30.3 ...
  ..$ exh.press    : num [1:3341] 98.3 98.3 98.3 98.4 98.3 ...
  ..$ amb.temp     : num [1:3341] 8.86 8.89 8.52 8.81 8.77 ...
  ..$ amb.press    : num [1:3341] 98.7 98.7 98.7 98.7 98.8 ...
  ..$ amb.humidity : num [1:3341] 21.6 21.9 21.6 21.2 21.2 ...
  ..$ velocity     : num [1:3341] 0.2 0.2 0.1 0.1 0.1 0.1 0.1 0.2 0.1 0.2 ...
  ..$ revolution   : num [1:3341] 53.6 17.9 25.9 53.6 29.8 ...
  ..$ option.1     : num [1:3341] -0.578 -0.13397 -0.16876 -0.12665 0.000916 ...
  ..$ option.2     : num [1:3341] 0.029 0.0174 0.0201 0.0183 0.0235 ...
  ..$ option.3     : num [1:3341] -0.000916 -0.000549 0 -0.000183 -0.000183 ...
  ..$ latitude     : num [1:3341] 53.8 53.8 53.8 53.8 53.8 ...
  ..$ longitude    : num [1:3341] 1.57 1.57 1.57 1.57 1.57 ...
  ..$ altitude     : num [1:3341] 114 114 114 114 114 ...
  ..$ gps.velocity : num [1:3341] 0.1 0 0.2 0.1 0.1 0.1 0.1 0.1 0.2 0.1 ...
  ..$ satellite    : num [1:3341] 6 6 6 6 6 6 6 6 6 6 ...
  ..$ n.s          : Factor w/ 1 level "N": 1 1 1 1 1 1 1 1 1 1 ...
  ..$ w.e          : Factor w/ 1 level "W": 1 1 1 1 1 1 1 1 1 1 ...
 $ units    :'data.frame':	1 obs. of  25 variables:
  ..$ time.stamp   : Factor w/ 1 level "Y-M-D H:M:S GMT": 1
  ..$ local.time   : Factor w/ 1 level "s": 1
  ..$ conc.co      : Factor w/ 1 level "vol%": 1
  ..$ conc.co2     : Factor w/ 1 level "vol%": 1
  ..$ conc.hc      : Factor w/ 1 level "ppmC6": 1
  ..$ conc.nox     : Factor w/ 1 level "ppm": 1
  ..$ afr          : Factor w/ 1 level "": 1
  ..$ exh.flow.rate: Factor w/ 1 level "L/min": 1
  ..$ exh.temp     : Factor w/ 1 level "degC": 1
  ..$ exh.press    : Factor w/ 1 level "kPa": 1
  ..$ amb.temp     : Factor w/ 1 level "degC": 1
  ..$ amb.press    : Factor w/ 1 level "kPa": 1
  ..$ amb.humidity : Factor w/ 1 level "%": 1
  ..$ velocity     : Factor w/ 1 level "km/h": 1
  ..$ revolution   : Factor w/ 1 level "rpm": 1
  ..$ option.1     : Factor w/ 1 level "V": 1
  ..$ option.2     : Factor w/ 1 level "V": 1
  ..$ option.3     : Factor w/ 1 level "V": 1
  ..$ latitude     : Factor w/ 1 level "degree": 1
  ..$ longitude    : Factor w/ 1 level "degree": 1
  ..$ altitude     : Factor w/ 1 level "m": 1
  ..$ gps.velocity : Factor w/ 1 level "km/h": 1
  ..$ satellite    : Factor w/ 1 level "": 1
  ..$ n.s          : Factor w/ 1 level "": 1
  ..$ w.e          : Factor w/ 1 level "": 1
 $ pems     : chr "Horiba OBS-1300"
 $ vehicle  : chr "unknown"
 $ fuel     : chr "unknown"
 $ constants:'data.frame':	1 obs. of  30 variables:
  ..$ log.rate        : num 1000
  ..$ delay.co        : num 3.2
  ..$ delay.co2       : num 3.3
  ..$ delay.hc        : num 3.9
  ..$ delay.nox       : num 1.6
  ..$ delay.afr       : num 1.6
  ..$ mm.c            : num 12
  ..$ mm.h            : num 1.01
  ..$ mm.0            : num 16
  ..$ mm.co           : num 28
  ..$ mm.co2          : num 44
  ..$ mm.nox          : num 46
  ..$ conc.o2         : num 20.6
  ..$ alpha.hc        : num 1.85
  ..$ alpha.exhaust.hc: num 1.85
  ..$ beta.oc         : num 0
  ..$ density.fuel    : num 0.725
  ..$ density.exhaust : num 1.2
  ..$ thc.c6          : num 10
  ..$ pitot.k         : num 2538
  ..$ pitot.z         : num 0.015
  ..$ setting.gps.port: num 0
  ..$ k.wgec          : num 3.5
  ..$ setting.velocity: num 1
  ..$ setting.720nox  : num 1
  ..$ setting.gps     : num 1
  ..$ setting.hc      : num 0
  ..$ setting.option  : Factor w/ 1 level "": 1
  ..$ setting.coco2hc : Factor w/ 1 level "": 1
  ..$ setting.afr     : Factor w/ 1 level "": 1
 $ history  : chr "initial file created using import.obs"
 - attr(*, "class")= chr "pems"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(pems.object)
## maybe str(pems.object) ; plot(pems.object) ...
}
\keyword{datasets}
