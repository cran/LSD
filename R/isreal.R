isreal <-
function(x) # vector
{
	if (!is.numeric(x)) stop("This is not a numeric vector")
	vec = !(is.na(x) | is.nan(x) | (x==Inf) | (x==-Inf))
	return(vec)
}

