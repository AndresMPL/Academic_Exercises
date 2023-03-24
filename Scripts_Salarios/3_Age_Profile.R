

#----------------------------------------------------
#              3. Age-Earning Profile
#----------------------------------------------------

p_load(lessR, boot)

dt_edad<- dt_final %>% select(age,college, estrato1, ingtot,experiencia, y_total_m_ha, Ingresos_laborales, age_squred, female)

reg_wage <- lm(dt_edad$Ingresos_laborales ~ dt_edad$age + I(dt_edad$age_squred))
stargazer(reg_wage, type="latex")
reg_wage

#Plot de la estimación edad-ingresos
#Intervalos de confianza

boot1 <- function(data, index){
        coef(lm(Ingresos_laborales ~ age + I(age_squred), data = dt_edad, subset = index))}

boot2 <- boot(dt_edad, boot1, R=1000)
boot3 <- t(rbind(boot2$t0, apply(boot2$t, 2, function(x) sd(x))))

coeffunct <- function(data, index){
              reg_age <- lm(Ingresos_laborales ~ age + I(age_squred), data = dt_edad, subset = index)
              coefs <- reg_age$coefficients
              b2 <- coefs[2]
              b3 <- coefs[3]
              peak_age <- -b2/(2*b3)
              return(peak_age)
              }

boot4 <- boot(dt_edad, coeffunct, R=1000)
boot5 <- t(rbind(boot4$t0, apply(boot4$t, 2, function(x) sd(x))))

alpha <- 0.05
min <- boot5[1,1]-qnorm(1-(alpha/2))*boot5[1,2]
max <- boot5[1,1]+qnorm(1-(alpha/2))*boot5[1,2]

b11 <- boot2$t0[1]
b12 <- boot2$t0[2]
b13 <- boot2$t0[3]


resul_age <- lm(Ingresos_laborales ~ age + I(age_squred), data=dt_edad)
summary(resul_age)


maximo <- resul_age$fitted.values
peak <- -(resul_age$coefficients[2])/(2*(resul_age$coefficients[3]))
dt_edad$p_edad <- resul_age$coefficients[1]+(resul_age$coefficients[2]*dt_edad$age)+(resul_age$coefficients[3]*I(dt_edad$age_squred))


perfil_edad_salario <- ggplot(dt_edad, aes(x=age, y=p_edad)) + 
  geom_point(color='#424242', size=0.9) +
  geom_vline(aes(xintercept=peak), linetype='dashed', size=1, color='darkblue')+
  geom_linerange(aes(xmin=min, xmax=max, y = max(maximo)), size=2, color='coral1')+labs(title = 'Figura 5:Perfil edad-salario',
  x = 'Edad', y = 'Salario') + ylim (0, 10) +  theme_light()


perfil_edad_salario

