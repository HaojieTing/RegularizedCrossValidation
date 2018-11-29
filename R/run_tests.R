library('RUnit')

test.suite <- defineTestSuite ("example",
                                dirs = file.path("tests"),
                                testFileRegexp = "^runit.+\\.R",
                                testFuncRegexp = "^test.+",
                                rngKind = "Marsaglia-Multicarry",
                                rngNormalKind = "Kinderman-Ramage")
 
test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)
#printHTMLProtocol(test.result)

# runTestFile(file.path(.path.package(package="RUnit"), "examples/runitc2f.r")) 一种简单的运行单元测试的方法