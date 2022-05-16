roc_with_ci <- function(roc) {
	ciobj <- ci.se(roc, specificities = seq(0, 1, l = 25))
	dat.ci <- data.frame(x = as.numeric(rownames(ciobj)),
											 lower = ciobj[, 1],
											 upper = ciobj[, 3])
	
	ggroc(obj) +
		theme_minimal() +
		geom_abline(
			slope = 1,
			intercept = 1,
			linetype = "dashed",
			alpha = 0.7,
			color = "grey"
		) + coord_equal() +
		geom_ribbon(
			data = dat.ci,
			aes(x = x, ymin = lower, ymax = upper),
			fill = "steelblue",
			alpha = 0.2
		) 
} 
r
