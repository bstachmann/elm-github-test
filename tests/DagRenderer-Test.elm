module DagRendererTest exposing (..)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
import Test.Runner.Html


main : Test.Runner.Html.TestProgram
main =
    [ testSomeThing
    ]
        |> concat
        |> Test.Runner.Html.run


testSomeThing : Test
testSomeThing =
    describe "Some Thing"
        [ test "this should succeed" <|
            \() -> Expect.equal "blah" "blah"
        ]
