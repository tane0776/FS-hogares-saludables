setwd("C:/Users/tusig/OneDrive - Universidad EAFIT/HS-YO")



hh_fisicas <- c("cocina_mejor","piso_mejor","bano_bald")
personales <- c("edad","mujeres_1","educacionninguno","educacionsecundaria_completa","educacionterciaria",
                "desempleo_1","empleado_con_contrato_formal_1","asalariados_1","hh_size","nino_0_a_5",
                "nino_6_a_18","adulto","hijos","padres","city","ing","gast","pobreza_modera_y_extrema_sisben",
                "educacionsecundaria_incompleta")
calidad <- c("intervenciones_bano","intervenciones_cocina","intervenciones_piso")

base_matching <- hogares[,c("hh_id","start_p10","tratamiento_control.x",hh_fisicas,personales,calidad)]

base_matching <- base_matching[!is.na(base_matching$bano_bald),]
base_matching <- base_matching[!is.na(base_matching$edad),]

base_matching$gast <- as.numeric(base_matching$gast)
base_matching$city <- as.numeric(base_matching$city)
base_matching$tratamiento_control.x <- as.numeric(base_matching$tratamiento_control.x)

#no matching
m_0 <- matchit(tratamiento_control.x ~ edad+mujeres_1+
                         educacionninguno+educacionsecundaria_completa+educacionterciaria+
                         desempleo_1+empleado_con_contrato_formal_1+asalariados_1+hh_size+nino_0_a_5+
                         nino_6_a_18+adulto+hijos+padres+city+ing+gast+pobreza_modera_y_extrema_sisben+
                         educacionsecundaria_incompleta+intervenciones_bano+intervenciones_cocina+intervenciones_piso, data = base_matching, method = NULL, distance = "glm")

#Fewer control units than treated units; not all treated units will get a match.

summary(m_0)

plot(m_0,type="hist")

plot(summary(m_0))


#Matchig 1:1
m_nearest_1 <- matchit(tratamiento_control.x ~ edad+mujeres_1+
                         educacionninguno+educacionsecundaria_completa+educacionterciaria+
                         desempleo_1+empleado_con_contrato_formal_1+asalariados_1+hh_size+nino_0_a_5+
                         nino_6_a_18+adulto+hijos+padres+city+ing+gast+pobreza_modera_y_extrema_sisben+
                         educacionsecundaria_incompleta+intervenciones_bano+intervenciones_cocina+intervenciones_piso, data = base_matching, method = "nearest", ratio = 1)

#Fewer control units than treated units; not all treated units will get a match.

summary(m_nearest_1)

plot(m_nearest_1,type="hist")

plot(summary(m_nearest_1))

cocina_mejor+piso_mejor+bano_bald+ 

#Otro
m_full_1 <- matchit(tratamiento_control.x ~ edad+mujeres_1+
                         educacionninguno+educacionsecundaria_completa+educacionterciaria+
                         desempleo_1+empleado_con_contrato_formal_1+asalariados_1+hh_size+nino_0_a_5+
                         nino_6_a_18+adulto+hijos+padres+city+ing+gast+pobreza_modera_y_extrema_sisben+
                         educacionsecundaria_incompleta+intervenciones_bano+intervenciones_cocina+intervenciones_piso, data = base_matching,method = "full", distance = "glm", link = "probit")

#Fewer control units than treated units; not all treated units will get a match.

summary(m_full_1)

plot(m_full_1 ,type="hist")


plot(summary(m_full_1))

#Matchig 1:1
m_optimal_1 <- matchit(tratamiento_control.x ~ cocina_mejor+piso_mejor+bano_bald+edad+mujeres_1+
                         educacionninguno+educacionsecundaria_completa+educacionterciaria+
                         desempleo_1+empleado_con_contrato_formal_1+asalariados_1+hh_size+nino_0_a_5+
                         nino_6_a_18+adulto+hijos+padres+city+ing+gast+pobreza_modera_y_extrema_sisben+
                         educacionsecundaria_incompleta+intervenciones_bano+intervenciones_cocina+intervenciones_piso, data = base_matching, method = "optimal")

#Fewer control units than treated units; not all treated units will get a match.

summary(m_optimal_1)

plot(m_optimal_1,type="hist")

plot(summary(m_optimal_1))
