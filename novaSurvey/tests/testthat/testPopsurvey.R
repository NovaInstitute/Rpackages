context("Testing popProjek in package popSurvey")

orig = eval(parse(text = 'structure(list(place = c("embalenhle", "", "", "emzinoni", 
"", "", "kwadela", "", "", "lebohang", "", ""), `sol energy coal ignition` = structure(c(1L, 
                  2L, NA, 1L, 2L, NA, 1L, 2L, NA, 1L, 2L, NA), .Label = c("N", 
                  "Y"), class = "factor"), PointEst = c(18545, 16859, 0, 3544, 
                  6766, 0, 625, 357, 0, 4276, 4632, 0), Lower = c(11460, 10035, 
                  0, 2104, 4981, 0, 422, 194, 0, 2675, 2984, 0), Upper = c(25369, 
                  23944, 5475, 5329, 8206, 1105, 788, 560, 146, 5924, 6233, 1186
                  )), .Names = c("place", "sol energy coal ignition", "PointEst", 
                  "Lower", "Upper"), row.names = c(NA, -12L), class = "data.frame")'))

x <- do.call("popProjek", toetsdata.popProjek())

d <- all.equal(orig, x)

state <- d

if(is.logical(state)) {
        message("test passed")
}else {
                message("test not passed")
                print(d)
                
}




