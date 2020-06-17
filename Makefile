# Search path
VPATH = data scripts

# Targets
.PHONY : site update_data update_site
site : update_data update_site
update_data :
	Rscript scripts/update_data.R
update_site :
	Rscript scripts/update_site.R

# Pattern rules
%.csv : %.R
	Rscript $<
