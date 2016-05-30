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
import Task
import Process



type alias Elapsed b =
  { since : Time
  , data  : Maybe b
  }


{-| The state of the debouncer
-}
type alias Model b =
  { elapsed : Maybe (Elapsed b)
  }

{-| The initial debouncer
-}
init : Model b
init =
  { elapsed = Nothing
  }

{-| To bounce the debouncer, just make multiple calls to `Bounce`.
-}
type Msg b
  = Bounce (Maybe b -> Maybe b)
  | Assign (Maybe b -> Maybe b) Time
  | Finish Time


{-| The main logic of the debouncer.
-}
update : Time
      -> (Maybe b -> Cmd a)
      -> Msg b
      -> Model b
      -> (Model b, Cmd (Result a (Msg b)))
update delay mainAction action model =
  case action of
    Bounce x ->
      case model.elapsed of
        Nothing ->
          ( model
          , Cmd.batch
              [ Task.perform
                  Debug.crash
                  (Ok << Assign x)
                  Time.now
              , Task.perform
                  Debug.crash
                  (Ok << Finish)
                  <| Process.sleep delay `Task.andThen` \_ -> Time.now
              ]
          )
        Just _  ->
          ( model
          , Task.perform
              Debug.crash
              (Ok << Assign x)
              Time.now
          )
    Assign f current ->
      ( { model | elapsed =
                    case model.elapsed of
                      Nothing ->
                        Just { since = current
                             , data = f Nothing
                             }
                      Just elap ->
                        Just { elap | since = current
                                    , data  = f elap.data
                             }
        }
      , Cmd.none
      )
    Finish current ->
      case model.elapsed of
        Nothing ->
          ( model
          , Cmd.none
          ) -- no-op
        Just elap ->
          let elapsed = current - elap.since
          in if elapsed < delay
          then ( model
               , Cmd.none
               )
          else ( init
               , Cmd.map Err <| mainAction elap.data
               )
