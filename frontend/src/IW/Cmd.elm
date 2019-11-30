-- This module contains function to work with 'Model', 'Cmd' and 'Sub'.

module IW.Cmd exposing
       ( withCmd
       , noCmd
       , noSub
       , delay
       )

import Process
import Task

withCmd : Cmd msg -> model -> (model, Cmd msg)
withCmd cmd model = (model, cmd)

noCmd : model -> (model, Cmd msg)
noCmd model = withCmd Cmd.none model

noSub : model -> Sub msg
noSub _ = Sub.none

-- Delay sending out the msg by given number of milliseconds
delay : Int -> msg -> Cmd msg
delay millis msg = Process.sleep (toFloat millis) |> Task.perform (\_ -> msg)
