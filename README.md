# Background

This project ([final paper](http://www.andrewheiss.com/research/Andrew%20Heiss%20-%20Explaining%20Support%20for%20Undemocratic%20Leaders%20in%20Democracies%20in%20the%20Middle%20East.pdf)) is a replication and extension of Jamal, Amaney, and Mark Tessler. "Attitudes in the Arab World." *Journal of Democracy* 19, no. 1 (January 2008): 97–110 and is part of a semester-long project for Duke's Political Science 733 course in Maximum Likelihood Estimation. 


# Data

Data for the original article comes from the first wave of the [Arab Barometer](http://www.arabbarometer.org/), conducted in 2006-07. The raw data for this replication was downloaded from [ICPSR](http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/26581), which restricts all restiribution of its data except when "[y]ou are collaborating with other AUTHORIZED USERS to analyze the data for research or instructional purposes." It may be that this project constitutes collaborative research or instructional purposes, but just to be legally safe, I have not included the full Barometer in this repository.

For the sake of universal replicability, `rebuild_data.R` takes the original ICPSR data and cleans it for this analysis, creating a standalone `Barometer.RData` file. If you can access the original ICPSR data, you should be able to precisely replicate my (and Jamal and Tessler's) results. 


# Files

* `rebuild_data.R` takes pre-downloaded ICPSR data and cleans it for use in R
* `replication.R` builds all models used in the analysis ([see raw model output](http://www.andrewheiss.com/research/733_replication.html))
* `extension.R` add parameters to the replicated model
* `graphic_functions.R` provides wrappers for repeated `ggplot` plots
* `figureX.R` create the figures used in the final paper
* `sandbox.R` contains random code I toyed with


# License

All files in this repository are free and open source research software and are provided under the MIT license.

Copyright (C) 2013 Andrew Heiss

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.