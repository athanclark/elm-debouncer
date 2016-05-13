module Debounce exposing
  ( Debouncer
  , initDebouncer
  , DebouncerResults
  , handleDebouncerResults
  , updateDebouncer
  , DebouncerMsg (Bounce)
  )

{-|

This is a delay-based debouncer, where given a _minimum_ delay and an action
to issue, we'll build a stateful component that will eventually issue the action
_once_, after being given a slew of requests within the delay timeframe.


## Debouncer State

@docs Debouncer

@docs initDebouncer


## Starting the Debouncer

@docs DebouncerMsg


## Evaluating the Debouncer

@docs updateDebouncer

@docs DebouncerResults

@docs handleDebouncerResults

-}

import Time exposing (Time)
import Task
import Process



{-| The state of the debouncer
-}
type alias Debouncer b =
  { since : Maybe (Time, b)
  }

{-| The initial debouncer
-}
initDebouncer : Debouncer b
initDebouncer =
  { since = Nothing
  }

{-| To bounce the debouncer, just make multiple calls to `Bounce`.
-}
type DebouncerMsg b
  = Bounce b
  | Assign b Time
  | Finish Time

{-| Representing either more messages needing to be handled in this component,
    or the message you've been trying to debounce.
-}
type DebouncerResults b a
  = More (DebouncerMsg b)
  | Done a

{-| By being able to convert a `DebouncerMsg` to a message we understand, we
    can turn a whole `DebouncerResults` to a message we understand.
-}
handleDebouncerResults : (DebouncerMsg b -> a) -> DebouncerResults b a -> a
handleDebouncerResults f m =
  case m of
    More a -> f a
    Done a -> a

{-| The main logic of the debouncer.
-}
updateDebouncer : Time
               -> (b -> a)
               -> DebouncerMsg b
               -> Debouncer b
               -> (Debouncer b, Cmd (DebouncerResults b a))
updateDebouncer delay mainAction action model =
  case action of
    Bounce x ->
      case model.since of
        Nothing ->
          ( model
          , Cmd.batch
              [ Task.perform
                  Debug.crash
                  (\t -> More <| Assign x t)
                  Time.now
              , Task.perform
                  Debug.crash
                  (\t -> More <| Finish t)
                  <| Process.sleep delay `Task.andThen` \_ -> Time.now
              ]
          )
        Just _  ->
          ( model
          , Task.perform
              Debug.crash
              (\t -> More <| Assign x t)
              Time.now
          )
    Assign x current ->
      ( { model | since = Just (current, x) }
      , Cmd.none
      )
    Finish current ->
      case model.since of
        Nothing ->
          ( model
          , Cmd.none
          ) -- no-op
        Just (oldCurrent,x) ->
          let elapsed = current - oldCurrent
          in if elapsed < delay
          then ( model
               , Task.perform
                   Debug.crash
                   (\t -> More <| Finish t)
                   <| Process.sleep (delay - elapsed) `Task.andThen` \_ -> Time.now
               )
          else ( initDebouncer
               , Task.perform
                   Debug.crash
                   (\x -> x)
                   <| Task.succeed <| Done <| mainAction x
               )
