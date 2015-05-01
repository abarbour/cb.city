library(dplyr)
fi <- "cb.palettes."
txtfi <- paste0(fi,"txt")
rdafi <- paste0(fi,"rda")
message("Reading ", txtfi, " and saving it to ", rdafi, "...")

read.table(txtfi, header=TRUE, colClasses=rep('character',3)) %>%
	mutate(pal.n = as.numeric(pal.num)) %>%
	select(-pal.num) -> cbcity

save(cbcity, file=rdafi)
