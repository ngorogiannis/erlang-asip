%% @author nikos

-module('asip').
-export(
   [
    start/0, 
    finish/0, 
    set_pin_mode/2, 
    digital_write/2, 
    digital_read/1,
    analog_write/2,
    analog_read/1,
    set_autoreport/1
   ]).
%-compile(export_all).

%% assumption: ports are always exactly 8 bits long.

%http://stackoverflow.com/questions/4517393/opening-a-device-file-using-erlang

% *** ASIP GENERIC CONSTANTS ***
-define(EVENT_HANDLER         ,  "@").
-define(ERROR_MESSAGE_HEADER  ,  "~").
-define(INFO_MESSAGE_HEADER  ,  "!").

% Usually followed by a time interval in milliseconds to set autoevent status 
% (time=0 means disable autoevents)
-define(AUTOEVENT_MESSAGE     ,  "A").

% Used to re-map pins
-define(REMAP_PIN_MESSAGE     ,  "M").

% A standard event (such as reporting distance, etc.)
-define(ASIP_EVENT            ,  "e").
% END OF ASIP GENERIC CONSTANTS

% *** DEFINITION OF ASIP CONSTANTS FOR I/O SERVICE ***
-define(IO_SERVICE   ,          "I").
-define(PIN_MODE     ,          "P").
-define(DIGITAL_WRITE,          "d").
-define(ANALOG_WRITE ,          "a").
-define(PORT_DATA    ,          "d").
-define(ANALOG_VALUE ,          "a").
-define(PORT_MAPPING ,          "M").

% Pin modes
%% -define(UNKNOWN_MODE          ,  0).
%% -define(INPUT_MODE            ,  1).
%% -define(INPUT_PULLUP_MODE     ,  2).
%% -define(OUTPUT_MODE           ,  3).
%% -define(ANALOG_MODE           ,  4).
%% -define(PWM_MODE              ,  5).
%% -define(RESERVED_MODE         ,  6).
%% -define(OTHER_SERVICE_MODE    ,  7).

translate_mode(unknown)         -> "0";
translate_mode(input)           -> "1";
translate_mode(input_pullup)    -> "2";
translate_mode(output)          -> "3";
translate_mode(analog)          -> "4";
translate_mode(pwm)             -> "5";
translate_mode(reserved)        -> "6";
translate_mode(other)           -> "7".

%% -define(HIGH ,                   1).
%% -define(LOW  ,                   0).
% *** END ASIP CONSTANTS FOR I/O SERVICE ***

% to avoid confusion with symbols in the code.
-define(COMMA, ",").
-define(SEMICOLON, ":").
-define(LBRACE, "{").
-define(RBRACE, "}").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% state access/modification macros/functions
% state is a map from atoms corresponding to fields (below) to values (generally maps)
-define(APM_FIELD, analog_pins_map_field).
-define(PMP_FIELD, port_map_field).
-define(DPM_FIELD, digital_pins_map_field).

% get/set analog pin map (pins->values) from/in state map
get_apm(State) -> maps:get(?APM_FIELD, State).
set_apm(Apm, State) -> maps:put(?APM_FIELD, Apm, State).

get_pmp(State) -> maps:get(?PMP_FIELD, State).
set_pmp(PMP, State) -> maps:put(?PMP_FIELD, PMP, State).

get_dpm(State) -> maps:get(?DPM_FIELD, State).
set_dpm(DPM, State) -> maps:put(?DPM_FIELD, DPM, State).

mk_state() -> #{ ?APM_FIELD => #{}, ?PMP_FIELD => #{}, ?DPM_FIELD => #{} }.

update_analog_pin(Pin, Val, State) ->
    set_apm(maps:put(Pin, Val, get_apm(State)), State).
    
get_analog_pin(Pin, State) -> maps:get(Pin, get_apm(State)).    

update_portmap(Port, BitPos, Pin, State) ->
    PMP = get_pmp(State),
    PortMap = maps:put(BitPos, Pin, maps:get(Port, PMP, #{})),
    set_pmp(maps:put(Port, PortMap, PMP), State).

update_digital_pin(BitPos, Pin, {Bitmask, DPM}) ->
    Val = 
        case BitPos band Bitmask of 
            0 -> 0;
            _ -> 1
        end,
    {Bitmask, maps:put(Pin, Val, DPM)}.

update_port_data(Port, Bitmask, State) ->
    DPM = get_dpm(State),
    PMP = get_pmp(State),
    case maps:get(Port, PMP) of
        {ok, PortMap} ->
            NewDPM = maps:fold(fun update_digital_pin/3, {Bitmask, DPM}, PortMap),
            set_dpm(NewDPM, State);
        error ->
            State
    end.

get_digital_pin(Pin, State) -> maps:get(Pin, get_dpm(State)).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utilities

log(S) -> io:format(standard_error, "LOG: " ++ S, []).
log(P, A) -> io:format(standard_error, "LOG: " ++ P, A).

trim_ends(B) -> binary:part(B, 1, byte_size(B) - 2).

upto(<< C, Rest/binary >>, << C >>) -> Rest;
upto(<< _, Rest/binary >>, << C >>) -> upto(Rest, << C >>).

number_pair({A, B}, Base) ->
    {binary_to_integer(A, Base), binary_to_integer(B, Base)}.

decimal_pair(Pair) -> number_pair(Pair, 10).

hex_pair(Pair) -> number_pair(Pair, 16).

% decode a binary encoding a semicolon-separated pair
decode_pair(Bin) ->
    [A, B] = binary:split(Bin, << ?SEMICOLON >>),
    {A, B}.

%decode a list of pairs separated by commas and ended by a right-brace
decode_pairs(Bin) ->
    BinPairs = binary:split(Bin, << ?COMMA >>, [global]),
    lists:map(fun decode_pair/1, BinPairs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% auxiliary functions for processing input

process_analog_values(Pair, State) ->
%%     log("pair=~p~n", [Pair]),
    {Pin, Val} = decimal_pair(Pair),
    update_analog_pin(Pin, Val, State).    

process_port_map_pair(Pair, {Pin, State}) ->
%%     log("pair=~p pin=~p~n", [Pair, Pin]),
    {Port, BitPos} = hex_pair(Pair),
%%     log("Decoded port=~p, bitpos=~p~n", [Port, BitPos]),    
    NewState = update_portmap(Port, BitPos, Pin, State),
    {Pin + 1, NewState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main input processing function

% process pin data change
process_input(
  << ?EVENT_HANDLER, ?IO_SERVICE, ?COMMA, ?PORT_DATA, ?COMMA, BinPair >>, State) ->
    log("Got port data: ~p~n", BinPair),
    {DecPort, HexBitmask} = decode_pair(BinPair),
    Port = binary_to_integer(DecPort, 10), 
    Bitmask = binary_to_integer(HexBitmask, 16),
    log("Port = ~B, Bitmask = ~.2B~n", [Port, Bitmask]),
    update_port_data(Port, Bitmask, State);

% process port mapping
process_input(
  << ?EVENT_HANDLER, ?IO_SERVICE, ?COMMA, ?PORT_MAPPING, ?COMMA, Data/binary >>, 
  State) ->
    % first of all get rid of length argument
    Rest = upto(Data, << ?COMMA >>), 
    BinPairs = trim_ends(Rest),
    Pairs = decode_pairs(BinPairs),
%%     log("Pairs: ~p~n", [Pairs]),
    {_, NewState} = lists:foldl(fun process_port_map_pair/2, {0, State}, Pairs),
%%     log("New state: ~p~n", [NewState]),
    NewState;

% set analog values 
process_input(
  << ?EVENT_HANDLER, ?IO_SERVICE, ?COMMA, ?ANALOG_VALUE, ?COMMA, Data/binary >>, State) ->
    BinValues = upto(Data, << ?COMMA >>),
    BinPairs = trim_ends(BinValues),
    Pairs = decode_pairs(BinPairs),
    lists:foldl(fun process_analog_values/2, State, Pairs);

% error message
process_input(<< ?ERROR_MESSAGE_HEADER, Data/binary >>, State) ->
    log("ERROR: ~s~n", [Data]),
    State;

% debug message
process_input(<< ?INFO_MESSAGE_HEADER, Data/binary >>, State) ->
    log("INFO: ~s~n", [Data]),
    State;

% ignore empty lines
process_input(<< >>, State) -> 
    State; 

% catchall handler
process_input(Data, State) ->
    log("UNKNOWN: ~s~n",[Data]),
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main loop and open/close

terminate(In, Out, Status) ->
    port_close(In),
    port_close(Out),
    halt(Status).

main_loop(In, Out, State) ->
    receive
        {In, {data, {_, Data}}} ->
%%             log("RECEIVED: ## ~s ##~n", [Data]),
            NewState = process_input(Data, State),
%%             log("New state: ~p~n", [NewState]),
            main_loop(In, Out, NewState);
        {wstring, String} ->
            port_command(Out, String), % this must be port_command, not !
            main_loop(In, Out, State);
        {analog_read, Pin, Proc} ->
            Proc ! get_analog_pin(Pin, State),
            main_loop(In, Out, State);
        {digital_read, Pin, Proc} ->
            Proc ! get_digital_pin(Pin, State),
            main_loop(In, Out, State);
        {In, eof} ->
            log("Got EOF from serial input.~n", []),
            terminate(In, Out, 1);
        close ->
            log("Shutting down mail loop...", []),
            terminate(In, Out, 0)
    end.

main_hook() ->
    Fifo = "/dev/ttyACM0",
    % icanon is required below to allow tty-style piping
    % we wouldn't need/want it if we could open the device file directly
    SttySettings = "10:0:18b1:0:3:1c:7f:15:4:0:0:0:11:13:1a:0:12:f:17:16:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0:0 icanon",
    Mode =  [eof, binary, stream, {line, 1024}],
    os:cmd("stty -F" ++ Fifo ++ " " ++ SttySettings),
    Out = open_port({spawn,"/bin/cat > " ++ Fifo}, [out | Mode]),
    In = open_port({spawn, "/bin/cat " ++ Fifo}, [in | Mode]),
    log("Main thread started.~n"),
    main_loop(In, Out, mk_state()).
    
start() -> 
    log("Starting main thread ...~n"),
    register(mainproc, spawn(fun () -> main_hook() end)),
    timer:sleep(200),
    request_port_mapping(),
    timer:sleep(200),
    request_port_mapping(),
    timer:sleep(200),
    request_port_mapping().

finish() ->
    mainproc ! close. % this must be !, not port_command
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% output functions

write_string(String) ->
%%     log("Write_string: ~s~n", [String]),
    mainproc ! {wstring, String}. % this must be !, not port_command

request_port_mapping() -> 
    write_string(?IO_SERVICE ++ ?COMMA ++ ?PORT_MAPPING ++ "\n").

set_pin_mode(Pin, Mode) ->
    write_string(?IO_SERVICE ++ ?COMMA ++ ?PIN_MODE ++ ?COMMA ++ 
                     integer_to_list(Pin) ++ 
                     ?COMMA ++ translate_mode(Mode) ++ "\n").

digital_write(Pin, Value) ->
    write_string(?IO_SERVICE ++ ?COMMA ++ ?DIGITAL_WRITE ++ ?COMMA ++ 
                     integer_to_list(Pin) ++ 
                     ?COMMA ++ integer_to_list(Value) ++ "\n").

analog_write(Pin, Value) ->
    write_string(?IO_SERVICE ++ ?COMMA ++ ?ANALOG_WRITE ++ ?COMMA ++ 
                     integer_to_list(Pin) ++ 
                     ?COMMA ++ integer_to_list(Value) ++ "\n").

set_autoreport(Timems) ->
    write_string(?IO_SERVICE ++ ?COMMA ++ ?AUTOEVENT_MESSAGE ++ ?COMMA ++ 
                     integer_to_list(Timems) ++ "\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% input functions
analog_read(Pin) ->
    mainproc ! {analog_read, Pin, self()},
    receive
        Val -> Val
    end.

digital_read(Pin) ->
    mainproc ! {digital_read, Pin, self()},
    receive
        Val -> Val
    end.

