summarize_metrics <- 
function(metric_list, data_is_binary=TRUE) {
	medians = sapply(metric_list, median)
	means = sapply(metric_list, mean)
	num_alternatives = length(metric_list)
	lowers = rep(NA, num_alternatives)
	uppers = rep(NA, num_alternatives)
	if (data_is_binary) {
		cis = sapply(metric_list, function(d) {prop.test(sum(d),length(d))$conf.int})
		lowers = cis[1,]
		uppers = cis[2,]
	} else {
		cis = sapply(metric_list, function(x) {as.vector(quantile(x,c(0.05,0.95)))})
		lowers = cis[1,]
		uppers = cis[2,]
	}
	
	return(data.frame(mean=means,median=medians,lower=lowers,upper=uppers))
}