%%%-------------------------------------------------------------------
%% @doc roman_numerals public API
%% @end
%%%-------------------------------------------------------------------

-module(roman_numerals_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    roman_numerals_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
