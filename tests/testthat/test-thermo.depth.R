context("Check the output values from thermo.depth")


test_that("thermo.depth returns correct values",{
	
	example_data = system.file('extdata', 'Sparkling.daily.wtr', package="rLakeAnalyzer")
	
	wtr               = load.ts(example_data, tz='UTC')
	expected          = load.ts(system.file('extdata', 'Sparkling.thermod.tsv', package="rLakeAnalyzer"), tz='UTC')
	seasonal_expected = load.ts(system.file('extdata', 'Sparkling.sthermod.tsv', package="rLakeAnalyzer"), tz='UTC')
	
	m.d = ts.thermo.depth(wtr, seasonal=FALSE)
	s.m.d = ts.thermo.depth(wtr, seasonal=TRUE)
	
	expect_equal(m.d$thermo.depth, expected$thermo.depth, tolerance=0.01)
	expect_equal(s.m.d$thermo.depth, seasonal_expected$thermo.depth, tolerance=0.01)

})