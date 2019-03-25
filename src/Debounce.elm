module Debounce exposing
    ( Model
    , init
    , Msg(..)
    , update
    )

{-| This is a delay-based debouncer, where given a _minimum_ delay and an action
to issue, we'll build a stateful component that will eventually issue the action
_once_, after being given a slew of requests within the delay timeframe.


## Debouncer State

@docs Model

@docs init


## Starting the Debouncer

@docs Msg


## Evaluating the Debouncer

@docs update

-}

import Process
import Task exposing (Task)
import Time exposing (Posix, toMillis, utc)


type alias Elapsed a =
    { since : Posix
    , cont : Cmd a
    }


{-| The state of the debouncer
-}
type alias Model a =
    { elapsed : Maybe (Elapsed a)
    }


{-| The initial debouncer
-}
init : Model a
init =
    { elapsed = Nothing
    }


{-| To bounce the debouncer, just make multiple calls to `Bounce`.
-}
type Msg a
    = Bounce (Cmd a)
    | Assign (Cmd a) Posix
    | Finish Posix


performLog : Task Never a -> Cmd a
performLog =
    Task.perform identity


mkCmd : a -> Cmd a
mkCmd =
    performLog << Task.succeed


{-| The main logic of the debouncer.
-}
update :
    Posix
    -> Msg a
    -> Model a
    -> ( Model a, Cmd (Result (Msg a) a) )
update delay action model =
    let
        delayToMillis =
            toFloat (toMillis utc delay)
    in
    case action of
        Bounce x ->
            ( model
            , Cmd.batch
                [ Cmd.map (Err << Assign x) <| performLog Time.now
                , Cmd.map (Err << Finish) <|
                    performLog <|
                        Task.andThen (\_ -> Time.now) (Process.sleep delayToMillis)
                ]
            )

        Assign x current ->
            ( { model
                | elapsed = Just { since = current, cont = x }
              }
            , Cmd.none
            )

        Finish current ->
            case model.elapsed of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just elap ->
                    let
                        elapsed =
                            toFloat (toMillis utc current) - toFloat (toMillis utc elap.since)
                    in
                    if elapsed < delayToMillis then
                        ( model
                        , Cmd.none
                        )

                    else
                        ( init
                        , Cmd.map Ok elap.cont
                        )
