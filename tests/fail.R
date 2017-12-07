library( "sampleSelection" )

try( selection( s ~ z1, y ~ x1, type = "w" ) )


# tobit 2

try( selection( s ~ z1, y ~ x1 ) )

try( selection( s ~ z1, y ~ x1, type = "2" ) )

try( selection( "s ~ z1", y ~ x1 ) )

try( selection( "s ~ z1", y ~ x1, type = "2" ) )

try( selection( ~ z1, y ~ x1 ) )

try( selection( ~ z1, y ~ x1, type = "2" ) )

try( selection( s ~ z1, "y ~ x1" ) )

try( selection( s ~ z1, "y ~ x1", type = "2" ) )

try( selection( s ~ z1, ~ x1 ) )

try( selection( s ~ z1, ~ x1, type = "2" ) )

try( selection( s ~ z1, list( y ~ x1, y ~ x1 ), type = "2" ) )


# tobit 5

try( selection( s ~ z1, list( y1 ~ x1, y2 ~ x1 ) ) )

try( selection( s ~ z1, list( y1 ~ x1, y2 ~ x1 ), type = "5" ) )

try( selection( "s ~ z1", list( y1 ~ x1, y2 ~ x1 ) ) )

try( selection( ~ z1, list( y1 ~ x1, y2 ~ x1 ) ) )

try( selection( s ~ z1, list( "y1 ~ x1", y2 ~ x1 ) ) )

try( selection( s ~ z1, list( ~ x1, y2 ~ x1 ) ) )

try( selection( s ~ z1, list( y1 ~ x1, "y2 ~ x1" ) ) )

try( selection( s ~ z1, list( y1 ~ x1, ~ x1 ) ) )

try( selection( s ~ z1, y1 ~ x1, type = "5" ) )

try( selection( s ~ z1, "y1 ~ x1", type = "5" ) )


# treatment effects models

try( selection( s ~ z1, y1 ~ s + x1, type = "treatment" ) )

try( selection( s ~ z1, y1 ~ x1, type = "treatment" ) )

try( selection( "s ~ z1", y1 ~ s + x1, type = "treatment" ) )

try( selection(  ~ z1, y1 ~ s + x1, type = "treatment" ) )

try( selection( s ~ z1, "y1 ~ s + x1", type = "treatment" ) )

try( selection( s ~ z1, ~ s + x1, type = "treatment" ) )

try( selection( s ~ z1, list( y1 ~ s + x1, y1 ~ s + x1 ), type = "treatment" ) )

