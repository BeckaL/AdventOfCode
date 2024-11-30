Solutions to Advent of Code in scala (current 2.13 because I started this a while ago)

Repo is in a bit of a hodgepodge state.

### Usage

To run a day, use run [day] [year] [part]

e.g.
`sbt run 7 2022 1`

to run part 1 of day 7 2022. The part is optional - omitting it will run both parts.


If test data is provided, it will first test the part against the test
data and answer, and only if this is correct will it run it against the real data (currently in /src/test/resources - really need to move this). This is to save running against the real data (which often takes longer esp if I've brute forced it :-D ) unless we have a reasonable degree of confidence that it works.


If no test data / answer is available for that day, set the test data to that part for none, which will skip straight to running on real data.

### Setting up a year

TODO: script to generate a year (shouldn't be difficult, just haven't been bothered so far)

Create a package in src/main/scala called AOC_[YEAR]. 
Copy a controller file into this package from another year and rename accordingly. Comment all the days as these won't yet compile.
Update main to import this controller, and change the default year in there.

Create a folder in /test/resources with the year. This will be where the input files go.

Either create or update session-cookie.txt with a new session cookie from the advent of code website.

### Setting up a day

Use the script: python3 make_day.py [year] [day]
e.g.
`python3 make_day.py 2022 1`

If you have a working session cookie (see section above) and that day has been released on the website, this will also fetch input data
for that day. If you have created the day file in advance, you can re-run this script just to retrieve the input data.

Uncomment the day from the year controller.

When reading through the instructions for the day, identify the test case. Copy this and the test answer into test data. For part two, do the
same - if the input data is the same, no changes required, if it's different you can set the testData2 val.