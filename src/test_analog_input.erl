%% @author nikos
%% @doc @todo Add description to analog_input.


-module(test_analog_input).
-export([start/0]).

-define(LED1, 11).
-define(LED2, 12).
-define(LED3, 13).

-define(POTPIN, 2).

setup() -> 
    asip:start(),  
    %% Setting 3 pins to OUTPUT_MODE
    asip:set_pin_mode(?LED1, output),
    asip:set_pin_mode(?LED2, output),
    asip:set_pin_mode(?LED3, output),
    asip:set_pin_mode(?POTPIN + 14, analog), % see racket file for explanation
    asip:digital_write(?LED1, 0),
    asip:digital_write(?LED2, 0),
    asip:digital_write(?LED3, 0),
    
    %% You need to set the reporting interval for analog pins (in ms)
    asip:set_autoreport(100).

loop() ->
    asip:digital_write(?LED1, 1),
    timer:sleep(100 + asip:analog_read(?POTPIN)),
    asip:digital_write(?LED1, 0),
    asip:digital_write(?LED2, 1),
    timer:sleep(100 + asip:analog_read(?POTPIN)),
    asip:digital_write(?LED2, 0),
    asip:digital_write(?LED3, 1),
    timer:sleep(100 + asip:analog_read(?POTPIN)),
    asip:digital_write(?LED3, 0),
    asip:digital_write(?LED1, 1),
    loop().

start() ->
    setup(),
    loop().




