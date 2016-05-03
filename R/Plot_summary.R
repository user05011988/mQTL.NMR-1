Plot_summary <-
function(res,title){
  
  pairs(res ,upper.panel= panel.cor, lower.panel=panel.smooth,cex = 1.5,diag.panel=panel.hist, main=title)
}
