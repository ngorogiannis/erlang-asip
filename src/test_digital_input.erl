%% @author nikos
%% @doc @todo Add description to digital_input.


-module(test_digital_input).

-export([start/0]).

%% A simple example reading an INPUT_PULLUP pin
%% and setting 3 pins to high accordingly

-define(LED1, 11).
-define(LED2, 12).
-define(LED3, 13).

%% Analog pins on Arduino UNO go from 14 to 19 
%% when you need to set the pin mode. However,
%% you need to use the analog number when reading
%% the value with analog-read.
%% This will be fixed in the future.
-define(inputPin, 2).

start() ->
    setup(),
    loop(0).

setup() -> 
    asip:start(),  
    timer:sleep(1000),
    %% Setting 3 pins to OUTPUT_MODE
    asip:set_pin_mode(?LED1, output),
    asip:set_pin_mode(?LED2, output),
    asip:set_pin_mode(?LED3, output),
    asip:set_pin_mode(?inputPin, input_pullup),
    timer:sleep(1000),
    asip:digital_write(?LED1, 0),
    asip:digital_write(?LED2, 0),
    asip:digital_write(?LED3, 0).

loop(OldInput) -> 
    CurInput = asip:digital_read(?inputPin),
    case OldInput of
        CurInput -> noaction;
        _ ->
            %% We only send messages when the state of the button changes
            %% (otherwise we'd flood the serial port)
            case CurInput of
                0 ->
                    %% it's a pull-up, so LOW means pressed            
                    asip:digital_write(?LED1, 1),
                    asip:digital_write(?LED2, 1),
                    asip:digital_write(?LED3, 1);
                _ ->
                    asip:digital_write(?LED1, 0),
                    asip:digital_write(?LED2, 0),
                    asip:digital_write(?LED3, 0)
            end
    end,
    timer:sleep(500),
    loop(CurInput).


