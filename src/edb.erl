%%%-------------------------------------------------------------------
%%% @author borja
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Aug 2018 13:08
%%%-------------------------------------------------------------------
-module(edb).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start/1, stop/0, create_tables/1, check_tables/1, 
		 delete_tables/0]).
-export([read/1, dirty_read/1, write/1, dirty_write/1, delete/1, 
		 dirty_delete/1, wait_until_saved/2]).

-type attributes_table() :: {
	Record_Identifier :: atom(),
	Record_Fields :: [Field :: atom()]
}.

-define(MNESIA_INIT_TRIALS, 10).
-define(MNESIA_TABLE_BASE_CONFIGURATION, 
	[
		{disc_copies, [node()]}, 
		{type, set}
	]).

-record(test_record, {id, data}).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts mnesia and checks the passed attibutes table matches 
%% the current tables.
%% @end
%%--------------------------------------------------------------------
-spec start(Attributtes_TableList :: [attributes_table()]) -> 
	ok.
start(Attributes_TableList) ->
	wait_mnesia(Attributes_TableList),
	check_tables(Attributes_TableList).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
stop() ->
	mnesia:stop().

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
create_tables(Attributes_TableList) ->
	mnesia:stop(),
	mnesia:create_schema([node()]),
	mnesia:start(),
	[mnesia:create_table(Name, [{attributes, Record_Info} | ?MNESIA_TABLE_BASE_CONFIGURATION])
	 || {Name, Record_Info} <- Attributes_TableList],
	ok.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
%%check_tables(Attributes_TableList) ->
%%	try check_tables_aux(Attributes_TableList)
%%	catch _:_ ->
%%		create_tables(Attributes_TableList),
%%		check_tables_aux(Attributes_TableList)
%%	end.
check_tables(Attributes_TableList) ->
	Tables = [Name || {Name, _} <- Attributes_TableList],
	try check_tables(Tables, Attributes_TableList) of
		_ -> ok
	catch
		_:Reason ->
			error("Database tables not correct, execute delete_tables() and create_tables(Attributes_TableList)"),
			exit(Reason)
	end.
check_tables([Table | Tables], Attributes_TableList) ->
	Attributes = mnesia:table_info(Table, attributes),
	{Table, Attributes} = lists:keyfind(Table, 1, Attributes_TableList),
	check_tables(Tables, Attributes_TableList);
check_tables([], _) ->
	ok.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
delete_tables() ->
	mnesia:stop(),
	ok = mnesia:delete_schema([node()]).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
write([]) -> ok;
write(WList) when is_list(WList) ->
	Ok_List = lists:duplicate(length(WList), ok),
	case mnesia:transaction(fun() -> [mnesia:write(W) || W <- WList] end) of
		{atomic, Ok_List} -> ok;
		Fail -> exit(Fail)
	end;
write(W) ->
	case mnesia:transaction(fun() -> mnesia:write(W) end) of
		{atomic, ok} -> ok;
		Fail -> exit(Fail)
	end.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
read([]) -> [];
read(IdList) when is_list(IdList) ->
	F = fun(Id) -> {_, Table} = Id, {Table, Id} end,
	Table_Ids = [F(Id) || Id <- IdList],
	case mnesia:transaction(fun() -> [mnesia:read({Table, Id}) || {Table, Id} <- Table_Ids] end) of
		{atomic, RList} -> correct_read_result(RList);
		Fail -> exit(Fail)
	end;
read(Id) ->
	{_, Table} = Id,
	case mnesia:transaction(fun() -> mnesia:read({Table, Id}) end) of
		{atomic, []} -> undefined;
		{atomic, [R]} -> R;
		Fail -> exit(Fail)
	end.
correct_read_result([[Result] | Rest]) -> [Result | correct_read_result(Rest)];
correct_read_result([[] | Rest])       -> [undefined | correct_read_result(Rest)];
correct_read_result([])                -> [].

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
delete(IdList) when is_list(IdList) ->
	F = fun(Id) -> {_, Table} = Id, {Table, Id} end,
	Table_Ids = [F(Id) || Id <- IdList],
	Ok_List = lists:duplicate(length(IdList), ok),
	case mnesia:transaction(fun() -> [mnesia:delete({Table, Id}) || {Table, Id} <- Table_Ids] end) of
		{atomic, Ok_List} -> ok;
		Fail -> exit(Fail)
	end;
delete(Id) ->
	{_, Table} = Id,
	case mnesia:transaction(fun() -> mnesia:delete({Table, Id}) end) of
		{atomic, ok} -> ok;
		Fail -> exit(Fail)
	end.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
dirty_write(W) ->
	mnesia:dirty_write(W).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
dirty_read(Id) ->
	{_, Table} = Id,
	case mnesia:dirty_read({Table, Id}) of
		[] -> undefined;
		[R] -> R
	end.

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
dirty_delete(Id) ->
	{_, Table} = Id,
	mnesia:dirty_delete({Table, Id}).

%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs


%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
wait_until_saved(Id, Element) ->
	wait_until_saved(Id, Element, 10).

wait_until_saved(Id, Element, Index) when Index > 0 ->
	case edb:read(Id) of
		Element -> ok;
		_Other -> wait_until_saved(Id, Element, Index - 1)
	end;
wait_until_saved(Id, _Element, _Index) ->
	error({"Element not saved", Id}).


%%====================================================================
%% Internal functions
%%====================================================================

% ......................................................................................................................
wait_mnesia(Attributes_TableList) ->
	mnesia:start(),
	MapTestRec = fun({RName, [_ | RFields]}) -> {RName, list_to_tuple([RName, test_id | RFields]), test_id} end,
	Tuples_TableElementId = [MapTestRec(Attributes) || Attributes <- Attributes_TableList],
	Check_Trans = fun() -> running_check_function(Tuples_TableElementId) end,
	wait_mnesia(?MNESIA_INIT_TRIALS, Check_Trans).

wait_mnesia(N, Check_Trans) ->
	case mnesia:transaction(Check_Trans) of
		{atomic, _} -> running;
		Fail ->
			N < 0 andalso error({"Mnesia not responding", Fail}),
			timer:sleep(20), wait_mnesia(N - 1, Check_Trans)
	end.

running_check_function(Tuples_TableElementId) ->
	[mnesia:write(Element) || {_, Element, _} <- Tuples_TableElementId],
	[mnesia:read({Table, Id}) || {Table, _, Id} <- Tuples_TableElementId].
% TODO: Check the returned element is correct
%%	[Element = mnesia:read({Table, Id}) || {Table, Id, Element} <- Attributes_TableList].

%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
single_rwd_elements_test_() ->
	% {setup, Where, Setup, Cleanup, Tests | Instantiator}
	[
		{"The internal function wait mnesia waits correclty until it is possible to write and read",
		 {setup, local, fun attribute_table_setup/0, fun no_stop/1, fun possible_to_write_on_mnesia/1}}
	].

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
attribute_table_setup() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	_Attributes_TableList = [
		{test_record, record_info(fields, test_record)}].

no_stop(_) ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	ok.

% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
possible_to_write_on_mnesia(Attributes_TableList) ->
	{inorder, [
		?_assertMatch(ok, mnesia:create_schema([node()])),
		?_assertMatch(ok, mnesia:start()),
		?_assertMatch([{atomic, ok}], create_test_tables(Attributes_TableList)),
		?_assertMatch(running, wait_mnesia(Attributes_TableList))
	]}.


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------
create_test_tables(Attributes_TableList) ->
	[mnesia:create_table(Name, [{attributes, Record_Info} | ?MNESIA_TABLE_BASE_CONFIGURATION])
	 || {Name, Record_Info} <- Attributes_TableList].




