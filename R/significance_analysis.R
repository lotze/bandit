# performs overall proportion test; if it is significant at alpha=0.05, performs pairwise tests to see where there are significant differences between proportions
# n should not be very different between the different groups under test; this is intended for simple analysis of split tests with multiple outcomes
# result rows are in the same order as the input x and n
significance_analysis <-
function(x, n) {
	num_groups = length(x)
	
	p = x/n
	my_rank = rank(-p)

	pt = prop.test(x=x, n=n)
	
	lower = rep(NA, num_groups)
	upper = rep(NA, num_groups)
	significance = rep(NA, num_groups)	
	best = rep(0, num_groups)
	
	b = best_binomial_bandit(x, n)

	if (pt$p.value < 0.05) {
		is_best = 1
		for (cur_rank in (1:(num_groups-1))) {
			cur_index = which(my_rank==cur_rank)
			comparison_index = which(my_rank==cur_rank+1)
			# compare to the next lower proportion
			pt = prop.test(x=x[c(cur_index, comparison_index)], n=n[c(cur_index, comparison_index)], conf.level = (1 - 0.05))
			significance[cur_index] = pt$p.value
			lower[cur_index] = pt$conf.int[1]
			upper[cur_index] = pt$conf.int[2]
			best[cur_index] = is_best
			if (pt$p.value < 0.05) {
				is_best = 0
			}
		}
	}
	
	return(data.frame(successes=x, totals=n, estimated_proportion=p, lower=lower, upper=upper, significance=significance, rank=my_rank, best=best, p_best=b))
}
