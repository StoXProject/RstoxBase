context("test-expandDT")

DT <- data.table::data.table(a = 1:3, b = list(1:2,3,4:6))
DTexpanded <- expandDT(DT)
answer <- data.table::data.table(a = rep(1:3, c(2,1,3)), b = 1:6)
expect_equal(DTexpanded, answer)
