#!/usr/bin/env Rscript

library(plumber)
pr <- plumb('DSBot.R')
pr$run(port=4567)
