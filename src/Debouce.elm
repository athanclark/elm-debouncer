module Debounce exposing
  ( Debouncer
  )

{-|
@docs Debouncer
-}

import Time exposing (Time)



type alias Elapsed =
  { diff  : Time
  , start : Time
  }

{-| The state of the debouncer
-}
type alias Debouncer =
  { isWorking : Bool
  , elapsed   : Maybe Elapsed
  }

initDebouncer : Debouncer
initDebouncer =
  { isWorking = False
  , elapsed   = Nothing
  }
