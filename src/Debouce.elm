module Debounce exposing
  ( Debouncer
  , initDebouncer
  , DebouncerResults
  , handleDebouncerResults
  , updateDebouncer
  , DebouncerMsg (Start)
  )

{-|
@docs Debouncer

@docs initDebouncer

@docs DebouncerResults

@docs handleDebouncerResults

@docs updateDebouncer

@docs DebouncerMsg
-}

import Time exposing (Time)
import Task
import Process



{-| The state of the debouncer
-}
type alias Debouncer =
  { since : Maybe Time
  }

{-| The initial debouncer
-}
initDebouncer : Debouncer
initDebouncer =
  { since = Nothing
  }

{-| To bounce the debouncer, just make multiple calls to `Start`.
-}
type DebouncerMsg
  = Start
  | AssignTime Time
  | Finish Time

{-| Representing either more messages needing to be handled in this component,
    or the message you've been trying to debounce.
-}
type DebouncerResults a
  = More DebouncerMsg
  | Done a

{-| By being able to convert a `DebouncerMsg` to a message we understand, we
    can turn a whole `DebouncerResults` to a message we understand.
-}
handleDebouncerResults : (DebouncerMsg -> a) -> DebouncerResults a -> a
handleDebouncerResults f m =
  case m of
    More a -> f a
    Done a -> a

{-| The main logic of the debouncer.
-}
updateDebouncer : Time
               -> a
               -> DebouncerMsg
               -> Debouncer
               -> (Debouncer, Cmd (DebouncerResults a))
updateDebouncer delay mainAction action model =
  case action of
    Start ->
      case model.since of
        Nothing ->
          ( model
          , Cmd.batch
              [ Task.perform
                  Debug.crash
                  (\t -> More <| AssignTime t)
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
              (\t -> More <| AssignTime t)
              Time.now
          )
    AssignTime current ->
      ( { model | since = Just current }
      , Cmd.none
      )
    Finish current ->
      case model.since of
        Nothing         -> Debug.crash "Somehow got in forbidden state x_x"
        Just oldCurrent ->
          let elapsed = current - oldCurrent
          in  if elapsed < delay
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
                       <| Task.succeed <| Done mainAction
                   )
