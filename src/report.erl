%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2019 20:41
%%%-------------------------------------------------------------------
-module(report).
-compile([export_all, nowarn_export_all]). %%TODO: To delete after build

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

-record(test_record, {id, data}).
-define(record_to_mapf(Record),
	fun(Val) ->
		maps:from_list(lists:zip(
			record_info(fields, Record),
			tl(tuple_to_list(Val))))
	end
).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
new(Name) when is_atom(Name) ->
	new(atom_to_list(Name));
new(Name) ->
	{ok, IoDevice} = file:open(Name ++ ".json", [write]),
	undefined = put(this_pid_report, {IoDevice, first}),
	file:write(IoDevice, <<"[">>).

new(Path, Name) when is_atom(Name) ->
	new(Path ++ "/" ++ atom_to_list(Name));
new(Path, Name) ->
	new(Path ++ "/" ++ Name).


%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
map(Map) ->
	case get(this_pid_report) of
		{IO, rest} -> IoDevice = IO, file:write(IoDevice, <<",">>);
		{IO, first} -> IoDevice = IO, put(this_pid_report, {IoDevice, rest})
	end,
	file:write(IoDevice, jsone:encode(
		Map#{
			timestamp => erlang:monotonic_time(millisecond),
			pid => format_pid(self())
		}
	)).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
format_reference(Reference) when is_reference(Reference) ->
	list_to_binary(ref_to_list(Reference)).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
format_pid(PId) when is_pid(PId) ->
	list_to_binary(pid_to_list(PId)).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
format_tuple(Tuple) when is_tuple(Tuple) ->
	[format_any(Element) || Element <- tuple_to_list(Tuple)].


%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
format_map(Map) when is_map(Map) ->
	maps:from_list([{format_key(Key), format_any(Value)} || {Key, Value} <- maps:to_list(Map)]).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
format_function(Function) when is_function(Function) ->
	list_to_binary(io_lib:write(Function)).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
format_list(List) when is_list(List) ->
	[format_any(Element) || Element <- List].

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
format_any(Any) ->
	if
		is_map(Any) -> format_map(Any);
		is_list(Any) -> format_list(Any);
		is_tuple(Any) -> format_tuple(Any);
		is_pid(Any) -> format_pid(Any);
		is_function(Any) -> format_function(Any);
		is_reference(Any) -> format_reference(Any);
		true -> Any
	end.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
format_key(Key) ->
	if
		is_pid(Key) -> format_pid(Key);
		is_reference(Key) -> format_reference(Key);
		is_list(Key) -> list_to_binary(io_lib:write(Key));
		is_tuple(Key) -> list_to_binary(io_lib:write(Key));
		is_function(Key) -> format_function(Key);
		is_map(Key) -> list_to_binary(io_lib:write(Key));
		true -> Key
	end.


%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
close() ->
	{IoDevice, _} = get(this_pid_report),
	file:write(IoDevice, <<"]">>),
	file:close(IoDevice).

%%====================================================================
%% Internal functions
%%====================================================================

% ......................................................................................................................
internal_function_example(_) ->
	none.

%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
simple_report_test_() ->
	% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"Possible to report a in file without error",
		 {setup, local, fun report_opening/0, fun report_closing/1, fun simple_state_report/1}}
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
report_opening() ->
	report:new(report_test).

report_closing(_) ->
	report:close().

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
simple_state_report(_) ->
	MapTest_record = ?record_to_mapf(test_record),
	DataMap = MapTest_record(#test_record{
		id   = this_id,
		data = [data_1, data_2, data_3]
	}),
	[
		?_assertMatch({_, _}, report:map(DataMap))
	].


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------





