## testthat structure


test_that("unflatten returns a matrix with expected dims",
          require(RJSONIO)
          ny.json <- fromJSON(system.file("inst/tests", 
                                          package = "preroll"))
          ny.out <- unflatten(ny.json)
          expect_that(ny.out, is_a("matrix"))
          expect_that(min(dim(ny.out)) == 0, is_false())
          expect_that(ncol(ny.out), is_identical_to(length(ny.json))))

          
          
          