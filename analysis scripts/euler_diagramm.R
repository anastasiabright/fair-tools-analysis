library(eulerr)

names <- c("F-UJI", "FAIR Enough", "FAIR-Checker", "FAIR EVA")
combinations <- combn(names, 3)

uniform_intersections <- euler(c("F-UJI" = 13, "FAIR-Enough" = 11, "FAIR-Checker" = 10, "FAIR-EVA" = 15,
                                 "F-UJI&FAIR-Enough" = 0, 
                                 "F-UJI&FAIR-Checker" = 0,
                                 "F-UJI&FAIR-EVA" = 2,
                                 "F-UJI&FAIR-Enough&FAIR-Checker" = 0,
                                 "F-UJI&FAIR-Enough&FAIR-EVA" = 3,
                                 "F-UJI&FAIR-Checker&FAIR-EVA" = 2,
                                 "FAIR-Enough&FAIR-Checker" = 0, 
                                 "FAIR-Enough&FAIR-EVA" = 0,
                                 "FAIR-Enough&FAIR-Checker&FAIR-EVA" = 2,
                                 "FAIR-Checker&FAIR-EVA" = 0,
                                 "F-UJI&FAIR-Enough&FAIR-Checker&FAIR-EVA" = 6),
                               shape = "ellipse")
plot(uniform_intersections,
     # labels = list(labels = c("A1, R1", #"F-UJI&FAIR EVA"
     #                          "F3, F4, A2", # "F-UJI&FAIR Enough&FAIR EVA"
     #                          "R1.2, R1.3", # "F-UJI&FAIR-Checker&FAIR EVA"
     #                          "A1.1, A1.2", # ja, "FAIR Enough&FAIR-Checker&FAIR EVA"
     #                          "F1, F2, I1, I2, I3, R1.1")),
     fills = list(fill = c(#"lavenderblush2", 
                           "lightblue2", 
                           #"lightsalmon",
                           "lightyellow", 
                           "lightgreen", 
                           #"lightpink",
                           "plum2")),
     quantities = list(type = "counts"),
     legend = list(side = "right"))


uniform_intersections_3 <- euler(c("F-UJI" = 13, "FAIR-Enough" = 11, "FAIR-Checker" = 10,
                                 "F-UJI&FAIR-Enough" = 0, 
                                 "F-UJI&FAIR-Checker" = 0,
                                 "FAIR-Enough&FAIR-Checker" = 0, 
                                 "F-UJI&FAIR-Enough&FAIR-Checker" = 6),
                               shape = "ellipse")
plot(uniform_intersections_3,
     fills = list(fill = c(#"lavenderblush2", 
       "lightblue2", 
       #"lightsalmon",
       "lightyellow", 
       "lightgreen", 
       #"lightpink",
       "plum2")),
     quantities = list(type = "counts"),
     legend = list(side = "right"))

