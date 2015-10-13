ppersp <-
function (z,ppm,title="",theta=-15, phi=15,r=50)
{
#  z<-lip.SRVlog10_1.female.ehk
  #map<-1:TotM
  map<-1:dim(z)[1]
  jet.colors <- colorRampPalette( c("blue","green","red"))
  color <- jet.colors(100)
  nrz <- nrow(z)
  ncz <- ncol(z)
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  facetcol <- cut(zfacet, 100)
  persp(map, as.numeric(ppm), z, theta = theta, phi = phi, r=r, col = color[facetcol], main=title,ticktype="detailed",nticks=10,  border = NA,xlab="Location",ylab="Shift",zlab="LOD")
}
