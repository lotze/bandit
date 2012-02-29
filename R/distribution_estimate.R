distribution_estimate <- 
function(v, data_is_binary=TRUE) {
	p_to_examine = seq(0,1,by=0.01)
	if (data_is_binary) {
		sums = sum(c(v,0))
		totals = if (length(v)  > 0) {length(v)} else {0}
		quantiles = qbeta(p_to_examine,sums+1,totals-sums+1)
	} else {
		quantiles = as.vector(quantile(v,p_to_examine,type=2))
	}
	x = rep(quantiles,each=2)
	mids = (quantiles[2:length(quantiles)] + quantiles[1:(length(quantiles)-1)])/2.0
	widths = quantiles[2:length(quantiles)] - quantiles[1:(length(quantiles)-1)]
	heights = 0.01/widths
	y = c(0,rep(heights,each=2),0)
	
	x=x[is.finite(y)]
	y=y[is.finite(y)]
	
	mids=mids[is.finite(heights)]
	widths=widths[is.finite(heights)]
	heights=heights[is.finite(heights)]
	
	return(list(quantiles=quantiles,x=x,y=y,mids=mids,heights=heights,widths=widths))
}