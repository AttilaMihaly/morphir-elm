module Morphir.Elm.IncrementalFrontendTests exposing (..)

import Expect
import Morphir.Elm.IncrementalFrontend exposing (parseModule)
import Test exposing (Test, describe, test)


parseModuleTests : Test
parseModuleTests =
    describe "parseModule"
        [ test "valid module"
            (\_ ->
                case parseModule "module Foo exposing (..)" of
                    Ok _ ->
                        Expect.pass

                    Err error ->
                        Expect.fail (Debug.toString error)
            )
        , test "invalid module"
            (\_ ->
                case parseModule "module _Foo exposing (..)" of
                    Ok _ ->
                        Expect.fail "This should have failed parsing"

                    Err error ->
                        error
                            |> Expect.equal
                                [ { location = { column = 8, row = 1 }
                                  , problems = [ "Expecting variable" ]
                                  }
                                ]
            )
        ]
