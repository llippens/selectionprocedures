# 0. Source R file ####
#source(paste0(here::here(), "/selectionprocedures.R"))

# 1. Save images ####
png(paste0(dir, "/img/", "cover image.png"), width = 40, height = 40/1.78, units = "cm", res = 500)
cover
dev.off()

png(paste0(dir, "/img/", "sackett_p_ordered.png"), width = 20, height = 25, units = "cm", res = 300)
sackett.p 
dev.off()

png(paste0(dir, "/img/", "sackett_bw_d_ordered.png"), width = 20, height = 26, units = "cm", res = 300)
sackett.bwd
dev.off()

png(paste0(dir, "/img/", "sackett_p_uncertainty.png"), width = 20, height = 20, units = "cm", res = 300)
sackett.pci
dev.off()

png(paste0(dir, "/img/", "sackett_p_bw_d.png"), width = 25, height = 25, units = "cm", res = 300)
sackett.p.bwd
dev.off()