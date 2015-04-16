running <-
function (spectrum,width = 0.00025,mid=30,base=10) 
{
        n = length(spectrum);
        out = spectrum;
        for(i in (mid+1):(mid+base)) {
                out <- out - (c(spectrum[(i+1):n],rep(0,i)) + c(rep(0,i),spectrum[1:(n-i)])) * mid/base
        }
        out <- out + 0.5*(c(spectrum[(mid+1):n],rep(0,mid)) + c(rep(0,mid),spectrum[1:(n-mid)]))
        for(i in 1:(mid-1)) {
                out <- out + c(spectrum[(i+1):n],rep(0,i)) + c(rep(0,i),spectrum[1:(n-i)])
        }
        out * width
}
