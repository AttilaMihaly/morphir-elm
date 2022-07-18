#!/bin/bash

#   Copyright 2022 Morgan Stanley
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.


#
# This script runs Elm tests with Antiques data in CSV format generated by GenerateAntiqueTestData.elm
#
# The output of the tests is written to CSV files which can be used as expected results in the
# corresponding Spark/Scala test for a particular rule in the Elm Antiques model.
#

TEST_OUTPUT_DIR=$(mktemp -d -t elm-tests-XXXXXXXXXX)

SPARK_TEST_DATA_DIR=../../test/src/spark_test_data
mkdir -p "$SPARK_TEST_DATA_DIR"

# Generate the input test data as a CSV file of Antique records
elm-test GenerateAntiqueTestData.elm > "$TEST_OUTPUT_DIR/generate_antique_test_data.txt"
cat "$TEST_OUTPUT_DIR/generate_antique_test_data.txt" \
    | grep -m1 antiques_data.csv \
    | sed -e 's?antiques_data.csv: \["??' -e 's?",",?\n?g' -e 's?"]??' > "$SPARK_TEST_DATA_DIR/antiques_data.csv"


# Add Elm triple quotes around the CSV data
echo -n '    """' | cat - "$SPARK_TEST_DATA_DIR/antiques_data.csv" > "$TEST_OUTPUT_DIR/antiques_data.csv.in"
echo '"""' >> "$TEST_OUTPUT_DIR/antiques_data.csv.in"

# Update src/AntiquesDataSource.elm source with the newly generated Antiques CSV data
cat ../src/AntiquesDataSource.elm \
    | sed -e '/^    """/,/^"""/d' \
    | sed -e "/^csvData =/ r $TEST_OUTPUT_DIR/antiques_data.csv.in" > "$TEST_OUTPUT_DIR/Temp.elm"
cp "$TEST_OUTPUT_DIR/Temp.elm" ../src/AntiquesDataSource.elm

elmTestOutputToCsv () {
    elm-test $1 > "$TEST_OUTPUT_DIR/$2.txt"
    cat "$TEST_OUTPUT_DIR/$2.txt" \
        | grep -m1 "antiques_expected_results_$2.csv" \
        | sed -e "s?antiques_expected_results_$2.csv: Ok \"??" -e 's?"??g' -e 's?\\r\\n?\n?g' \
        > "$SPARK_TEST_DATA_DIR/antiques_expected_results_$2.csv"
}

# Run the is_item_vintage test and save the corresponding CSV file
elmTestOutputToCsv "TestIsItemVintage.elm" "is_item_vintage"

# Run the is_item_worth_millions test and save the corresponding CSV file
elmTestOutputToCsv "TestIsItemWorthMillions.elm" "is_item_worth_millions"

# Run the is_item_worth_thousands test and save the corresponding CSV file
elmTestOutputToCsv "TestIsItemWorthThousands.elm" "is_item_worth_thousands"

# Run the is_item_antique test and save the corresponding CSV file
elmTestOutputToCsv "TestItemIsAntique.elm" "is_item_antique"
