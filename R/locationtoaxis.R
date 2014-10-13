locationtoaxis <-
function (Tppm) 
{
traitl <- t(c((Tppm-5)/5.4 , 0 ))
return(traitl)
}
