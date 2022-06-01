import SimpleMarsRover (execute)
import Test.HUnit (Counts, Test (TestCase, TestList), assertEqual, runTestTT)

main :: IO Counts
main =
  runTestTT $
    TestList
      ( map
          ( \(input, output) ->
              TestCase
                ( assertEqual
                    ("execute " <> input <> " should return " <> output)
                    (execute input)
                    output
                )
          )
          [ ("L", "0:0:W"),
            ("LL", "0:0:S"),
            ("LLL", "0:0:E"),
            ("LLLL", "0:0:N"),
            ("R", "0:0:E"),
            ("RR", "0:0:S"),
            ("RRR", "0:0:W"),
            ("RRRR", "0:0:N"),
            ("MMMMMMMMMM", "0:0:N"),
            ("RMMMMMMMMMM", "0:0:E"),
            ("LMMMMMMMMMM", "0:0:W"),
            ("LLMMMMMMMMMM", "0:0:S"),
            ("MMRMMLM", "2:3:N"),
            ("RMMLM", "2:1:N")
          ]
      )