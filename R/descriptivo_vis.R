descriptivos <- function(datos, variable, titulo=""){
    paq <- as.list(match.call())[-1]
    paq <- sapply(paq, paste)[2]
    texto <- paste("datos$", paq, sep="")
    variable <- eval(parse(text = texto))

    densidades <- density(variable)

    pos_mediana <- which(densidades$x>=quantile(variable, .25) & densidades$x <= quantile(variable, .75))

    sigmas <- function(var){
        mediana <- median(var)
        c(-4*sd(var)+mediana,-3*sd(var)+mediana,-2*sd(var)+mediana,-sd(var)+mediana,
          mediana,
          sd(var)+mediana, 2*sd(var)+mediana, 3*sd(var)+mediana, 4*sd(var)+mediana)
    }

    grafico <- ggplot(mapping = aes(x=variable))+
        geom_density(color="red", fill="deepskyblue1")+
        scale_x_continuous(expand = c(0, 0), limits=range(densidades$x),
                           sec.axis = dup_axis(name="",
                                               breaks = c(sigmas(variable)),
                                               labels = c(expression(paste(-4, sigma)),
                                                          expression(paste(-3, sigma)),
                                                          expression(paste(-2, sigma)),
                                                          expression(paste(-1, sigma)),
                                                          expression(paste(0, sigma)),
                                                          expression(paste(1, sigma)),
                                                          expression(paste(2, sigma)),
                                                          expression(paste(3, sigma)),
                                                          expression(paste(4, sigma)))))+
        scale_y_continuous(expand = c(0, 0))+
        theme_classic()+
        theme(axis.text.y=element_blank(),
              axis.line.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major.x = element_line(linetype = "longdash", color = "black"))+
        geom_area(data = with(densidades, data.frame(x, y))[pos_mediana,],
                  aes(x=x, y=y),
                  fill="dodgerblue4")+
        geom_vline(xintercept = c(min(variable),
                                  quantile(variable, probs = c(.25,.75)),
                                  max(variable)),
                   linetype="twodash", color = "blue")+
        geom_vline(xintercept = sigmas(variable), linetype="dashed", color="darkorange2")+
        labs(title=titulo)

    estadisticos <- data.frame(Mínimo=min(variable), Q1=quantile(variable, .25), Mediana=quantile(variable, .5),
                               Q3=quantile(variable, .75), Máximo=max(variable), s=sd(variable),
                               Kurtosis=kurtosis(variable), Skweness=skewness(variable)) %>%
        mutate_all(round, 2) %>%
        ggtexttable(., rows = NULL,
                    theme = ttheme("mBlue"))

    ggarrange(grafico, estadisticos,
              ncol = 1, nrow = 2,
              heights = c(3, 0.5))
}
