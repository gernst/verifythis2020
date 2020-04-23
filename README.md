# VerifyThis2020

Material and solutions for the VerifyThis long term challenge 2020 https://verifythis.github.io


## Running the test

The current solution contains one general test which compares the high-level History 
run against an equivalent run using the pgp server model.

The checks are done using ScalaCheck, which is also responsible for generating arbitrary 
sequences of Upload/Verify/Revoke actions.

The current test can be found under `src/test/scala/HistoryExecutionSpec`.

To build the project and run the test execute `sbt test` in the project root directory.