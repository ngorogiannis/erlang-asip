%% @author nikos
%% @doc @todo Add description to blinkled.


-module(test_blinkleds).
-export([start/0]).

-define(LED1, 11).
-define(LED2, 12).
-define(LED3, 13).

loop() ->
    asip:digital_write(?LED1, 1),
    timer:sleep(1000),
    asip:digital_write(?LED1, 0),
    asip:digital_write(?LED2, 1),
    timer:sleep(1000),
    asip:digital_write(?LED2, 0),
    asip:digital_write(?LED3, 1),
    timer:sleep(1000),
    asip:digital_write(?LED3, 0),
    asip:digital_write(?LED1, 1),
    loop().   

start() ->
    asip:start(),
    asip:set_pin_mode(?LED1, output),
    asip:set_pin_mode(?LED2, output),
    asip:set_pin_mode(?LED3, output),
    asip:digital_write(?LED1, 0),
    asip:digital_write(?LED2, 0),
    asip:digital_write(?LED3, 0),
    loop().

