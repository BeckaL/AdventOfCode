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

