module Out exposing (Msg(..))

import Status
import Constants


--
-- Messages to out to the top level
--


type Msg
  = StatusMsg Status.Msg
  | RequestTag Constants.Tag


