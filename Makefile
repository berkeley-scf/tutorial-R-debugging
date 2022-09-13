index.md: index.Rmd
	Rscript -e "rmarkdown::render('index.Rmd', rmarkdown::md_document(preserve_yaml = TRUE, variant = 'gfm'))"

