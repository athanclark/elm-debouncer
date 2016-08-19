module Debounce exposing
  ( Model
  , init
  , update
  , Msg (Bounce)
  )

{-|

This is a delay-based debouncer, where given a _minimum_ delay and an action
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

import Time exposing (Time)
import Task exposing (Task)
import Process



type alias Elapsed a =
  { since : Time
  , cont  : Cmd a
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
  | Assign (Cmd a) Time
  | Finish Time


performLog : Task e a -> Cmd a
performLog = Task.perform (Debug.crash << toString) identity

mkCmd : a -> Cmd a
mkCmd = performLog << Task.succeed


{-| The main logic of the debouncer.
-}
update : Time
      -> Msg a
      -> Model a
      -> (Model a, Cmd (Result (Msg a) a))
update delay action model =
  case action of
    Bounce x ->
      case model.elapsed of
        Nothing ->
          model ! [ Cmd.map (Err << Assign x) <| performLog Time.now
                  , Cmd.map (Err << Finish)
                 <| performLog <| Process.sleep delay `Task.andThen` \_ -> Time.now
                  ]
        Just _  ->
          model ! [ Cmd.map (Err << Assign x) <| performLog Time.now
                  ]
    Assign x current ->
      { model | elapsed = Just { since = current, cont = x }
      } ! []
    Finish current ->
      case model.elapsed of
        Nothing -> model ! []
        Just elap ->
          let elapsed = current - elap.since
          in if elapsed >= delay
          then model ! []
          else init  ! [ Cmd.map Ok elap.cont ]
