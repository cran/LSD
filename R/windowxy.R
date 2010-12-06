windowxy <-
function(windows=1) # number of windows
{
	if (is.prim(windows) & windows <= 3) return(c(1,windows))
	if (is.prim(windows) & windows > 3){a = floor(sqrt(windows))
		if (a^2==windows) return(c(a,a))
		if ( windows <= a*(a+1) ) return(c(a,a+1))
		return(c(a,a+2))
	}
	if (!is.prim(windows)){p = prime.factor(windows)
		if (length(p) == 2) return(p)
		else {while(length(p) > 2) {l = length(sort(p))
				s = sort(p)
				p = c(s[1]*s[2],s[3:length(s)])
			}
			return(sort(p))
		}
	}                                    
}

