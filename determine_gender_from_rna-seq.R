genderPlots <- function(genders, counts, samples) {
	# Making orthogonal gender-specific plot based on genes from https://www.ncbi.nlm.nih.gov/pubmed/23829492
	maleGenes <- c('ENSG00000129824', 'ENSG00000198692', 'ENSG00000067048', 'ENSG00000012817')
	femaleGenes <- c('ENSG00000229807')
	if (any(maleGenes %in% rownames(counts))){
		maleCounts <- rowSums(t(counts[rownames(counts) %in% maleGenes,]))
	} else {
		maleCounts <- rep(0, length(genders))
	}
	if (any(femaleGenes %in% rownames(counts))){
		femaleCounts <- counts[rownames(counts) %in% femaleGenes,]
	} else {
		femaleCounts <- rep(0, length(genders))
	}
	data <- data.frame(
		m=maleCounts,
		f=femaleCounts,
		gender=genders,
		name=samples)
	p <- ggplot(data = data, aes(x=f, y=m, colour=gender)) +
		geom_point() +
		ggtitle("Reads in gender specific genes") +
		theme(axis.ticks.x=element_blank(),
			panel.grid.major.x = element_blank(),
			plot.title = element_text(hjust = 0.5),
			legend.position="none") +
		ylab("Raw number of reads in male specific genes") +
		xlab("Raw number of reads in female specific gene") +
		geom_text_repel(aes(label=name), size=3)
		suppressMessages(ggsave("GenderSpecificExpression.jpeg", p))
}
