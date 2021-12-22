library(DramaAnalysis)

gdc_ids <- c("11g3h.0", "11f78.0", "11hdv.0", "nsv5.0", "pkx0.0", "qdjz.0", "qd2g.0", 
             "r12v.0", "r0px.0", "rhzq.0", "rksp.0", "rkpt.0", "t25v.0", 
             "v0d8.0", "v0fv.0", "v3sw.0", "nm24.0", "pb4c.0", "pb0f.0", "x4vf.0")
gdc_dramanames <- c("goethe-die-natuerliche-tochter", "goethe-iphigenie-auf-tauris",
                    "goethe-stella", "grillparzer-die-ahnfrau", "hebbel-maria-magdalene", 
                    "hofmannsthal-der-rosenkavalier", "hofmannsthal-elektra", "kleist-die-familie-schroffenstein", 
                    "klinger-die-zwillinge", "lenz-der-hofmeister", "lessing-emilia-galotti", 
                    "lessing-nathan-der-weise", "pfeil-lucie-woodvil", 
                    "schiller-die-braut-von-messina", "schiller-die-raeuber", 
                    "schnitzler-komtesse-mizzi", "gottschedin-das-testament", 
                    "guenderode-magie-und-schicksal", "guenderode-udohla", 
                    "weissenthurn-das-manuscript")
id_map <- data.frame(gdc_ids, gdc_dramanames)
da_dramas <- loadDrama(gdc_ids, defaultCollection = "gdc")
