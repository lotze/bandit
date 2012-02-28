distribution_estimate <- 
function(metric_list, data_is_binary=TRUE) {
	p_to_examine = seq(0,1,by=0.01)
	if (data_is_binary) {
		sums = sapply(metric_list, function(v) {sum(c(v,0))})
		totals = sapply(metric_list, function(v) {if (length(v)  > 0) {length(v)} else {0}})
		retval = sapply(1:length(metric_list), function(i) {qbeta(p_to_examine,sums[i]+1,totals[i]-sums[i]+1)})
	} else {
		retval = sapply(metric_list, function(x) {as.vector(quantile(x,p_to_examine))})
	}
	return(retval)
}