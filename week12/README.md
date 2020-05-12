# Week 12: Multiple Linear Regression

Today we'll extend our Linear Regression to include multiple explanatory variables - Multiple Linear Regression.  We'll also talk about some of the issues that can arise with trusting p-values alone and address issues of "p-hacking" and "data dredging".

## Class outline

 1. [Lecture](lecture12_s2020_toupload.pdf)
 1. [Notebook #1: Starting Multiple Linear Regression](prep_intro_mlr_part1.ipynb)
	* See [as an Rscript](Rscripts/prep_week12_intro_mlr.R)	
	* Data:  beersbac.csv([view](../week11/beersbac.csv), [raw](https://raw.githubusercontent.com/jnaiman/is542_spring2020/master/week11/beersbac.csv))
 1. [Notebook #2: Hubbles Law using Linear Regression](prep_hubblesExample_part2.ipynb)
	* See [as an Rscript](Rscripts/prep_usingANOVA_week09_part2.R)	
	* Data:  hubble.csv([view](hubble.csv), [raw](https://raw.githubusercontent.com/jnaiman/is542_spring2020/master/week11/hubble.csv))
	
**Extra:** Hubble's data processing (Python) and original data [linked here](fullHubbleData).

## Reading

Multiple Linear Regression: OIS 8.1-8.3 & ISL 3.2-3.5

**Optional** Reading: MIS Ch. 8.1-8.3; Ch 2.8 (issues with p-values)

Packages to install (with command: install.packages("PACKAGE_NAME") or install.packages(PACKAGE_NAME) ):
  1. psych
  2. effects (Note: this may not install on all machines, but don't worry it is not essential)
  3. corrplot
  4. car
  5. manipulate
  6. tigerstats

## [Homework](homework.md)

## References
 
 * Montana's course - http://www.math.montana.edu/courses/s217
 * Hubble's law - http://adamdempsey90.github.io/python/dark_energy/dark_energy.html
