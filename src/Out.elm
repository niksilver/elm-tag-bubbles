module Out exposing (Msg(..))

import Status


--
-- Messages to out to the top level
--


type Msg
  = StatusMsg Status.Msg


