%%%-------------------------------------------------------------------
%%% File    : example_SUITE.erl
%%% Author  :
%%% Description :
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(nndb_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").

-define(INFO(Info), ct:log(?LOW_IMPORTANCE, "Info report: ~p", [Info])).
-define(ERROR(Error), ct:pal(?HI_IMPORTANCE, "Error report: ~p", [Error])).

-record(test_rectab, {id, data}).
-define(MNESIA_TEST_TABLE_ATTRIBUTES_LIST, [
	{test_rectab, record_info(fields, test_rectab)}]).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
	[{timetrap, {seconds, 30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
	nndb:create_tables(?MNESIA_TEST_TABLE_ATTRIBUTES_LIST),
	nndb:start(?MNESIA_TEST_TABLE_ATTRIBUTES_LIST),
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
	nndb:stop(),
	ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
	ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
	Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
	ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
	[].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
	[
		test_nndb_individual_elements,
		test_nndb_multiple_elements
	].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
my_test_case_example() ->
	[].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
my_test_case_example(_Config) ->
	ok.

% --------------------------------------------------------------------
% COMMON TESTS -------------------------------------------------------
% ......................................................................................................................
test_nndb_individual_elements() ->
	[].
test_nndb_individual_elements(_Config) ->
	Element_1 = #test_rectab{id = {{0, make_ref()}, test_rectab}}, ?INFO(Element_1),
	Element_2 = #test_rectab{id = {{0, make_ref()}, test_rectab}}, ?INFO(Element_2),
	% Test writing
	{'EXIT', {aborted, {bad_type, bad_thing}}} = (catch nndb:write(bad_thing)),
	ok = nndb:write(Element_1),
	% Test reading
	undefined = nndb:read(Element_2#test_rectab.id),
	{'EXIT', {{badmatch, bad_thing}, _}} = (catch nndb:read(bad_thing)),
	Element_1 = nndb:read(Element_1#test_rectab.id),
	% Test deleting
	{'EXIT', {{badmatch, bad_thing}, _}} = (catch nndb:delete(bad_thing)),
	Element_1 = nndb:read(Element_1#test_rectab.id), ?INFO(Element_1),
	ok = nndb:delete(Element_1#test_rectab.id),
	undefined = nndb:read(Element_1#test_rectab.id), ?INFO(Element_1),
	ok = nndb:delete(Element_1#test_rectab.id), ?INFO(Element_1).


% ......................................................................................................................
test_nndb_multiple_elements() ->
	[].
test_nndb_multiple_elements(_Config) ->
	Element_1 = #test_rectab{id = {{0, make_ref()}, test_rectab}}, ?INFO(Element_1),
	Element_2 = #test_rectab{id = {{0, make_ref()}, test_rectab}}, ?INFO(Element_2),
	Element_3 = #test_rectab{id = {{0, make_ref()}, test_rectab}}, ?INFO(Element_3),
	Elements_2_3 = [Element_2, Element_3], ?INFO(Elements_2_3),
	Elements_1_2_3 = [Element_1, Element_2, Element_3], ?INFO(Elements_1_2_3),
	% Test writing in groups
	{'EXIT', {aborted, {bad_type, _}}} = (catch nndb:write([bad_thing1, bad_thing2])),
	ok = nndb:write(Elements_2_3),
	% Test reading in groups
	{'EXIT', {{badmatch, _}, _}} = (catch nndb:read([bad_thing1, bad_thing2])),
	[undefined, Element_2, Element_3] = nndb:read([E#test_rectab.id || E <- Elements_1_2_3]),
	ok = nndb:write(Elements_1_2_3),
	Elements_1_2_3 = nndb:read([E#test_rectab.id || E <- Elements_1_2_3]),
	% Test deleting in groups
	{'EXIT', {{badmatch, _}, _}} = (catch nndb:delete([bad_thing1, bad_thing2])),
	ok = nndb:delete([E#test_rectab.id || E <- Elements_2_3]),
	[Element_1, undefined, undefined] = nndb:read([E#test_rectab.id || E <- Elements_1_2_3]),
	ok = nndb:delete([E#test_rectab.id || E <- Elements_1_2_3]),
	[undefined, undefined, undefined] = nndb:read([E#test_rectab.id || E <- Elements_1_2_3]),
	ok = nndb:delete([E#test_rectab.id || E <- Elements_1_2_3]),
	[undefined, undefined, undefined] = nndb:read([E#test_rectab.id || E <- Elements_1_2_3]),
	ok.


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------

