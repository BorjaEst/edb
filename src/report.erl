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

-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, start/0, new/1, write/2, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(EUNIT_TEST_FILE, "eunit_report_test.json").
-record(state, {}).


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the log server with a link (for supervisors).
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Starts the log server
%% @end
%%--------------------------------------------------------------------
-spec(start() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).


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
	gen_server:call(?SERVER, {new, Name}).


%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
write(Ref, EJSON) ->
	EJSON_Ext = EJSON#{
			<<"timestamp">> => erlang:monotonic_time(millisecond),
			<<"pid">> => erlang:term_to_binary(self())
	},
	gen_server:cast(?SERVER, {write, Ref, EJSON_Ext}).

	
%%--------------------------------------------------------------------
%% @doc
%%
%%
%% @end
%%--------------------------------------------------------------------
%TODO: Correct specs
close(Ref) ->
	gen_server:call(?SERVER, {close, Ref}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
	process_flag(trap_exit, true),
	put(io_devices, #{}),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({new, File}, _From, State) ->
	case file:open(File, [write]) of
		{ok, IoDevice} ->
			Reply = {ok, add_iodevice(IoDevice)};
		{error, Reason} ->
			Reply = {error, Reason} 
	end,
	{reply, Reply, State};

handle_call({close, Ref}, _From, State) ->
	Reply = del_iodevice(Ref),
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	?LOG_INFO("Unexpected message"),
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({write, Ref, EJSON}, State) ->
	write_iodevice(Ref, EJSON),
	{noreply, State};
handle_cast(_Request, State) ->
	?LOG_INFO("Unexpected message"),
  	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
	?LOG_INFO("Unexpected message"),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	[del_iodevice(Ref) || Ref <- maps:keys(get(io_devices))],
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    			  Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	?LOG_INFO("Unexpected message"),
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% ....................................................................
add_iodevice(IoDevice) ->
	IoDev_Map = get(io_devices),
	Ref = make_ref(),
	put(io_devices, IoDev_Map#{Ref => {IoDevice, next_is_val}}),
    file:write(IoDevice, <<"[">>),
	Ref.


% ....................................................................
del_iodevice(Ref) ->
    IoDev_Map = get(io_devices),
	{IoDevice, _} = maps:get(Ref, IoDev_Map),
	file:write(IoDevice, <<"[">>),
	put(io_devices, maps:remove(Ref, IoDev_Map)),
	file:close(IoDevice).


% ....................................................................
write_iodevice(Ref, EJSON) ->
	IoDev_Map = get(io_devices),
	UpdateFun = fun(Val) -> write_ejson(Val, EJSON) end,
    put(io_devices, 
		maps:update_with(Ref, UpdateFun, IoDev_Map)).

write_ejson({IoDevice, next_is_val}, EJSON) ->
	Json = jiffy:encode(EJSON),
	file:write(IoDevice, Json),
	{IoDevice, next_is_separator};
write_ejson({IoDevice, next_is_separator}, EJSON) ->
	file:write(IoDevice, <<",">>),
	write({IoDevice, next_is_val}, EJSON).


%%====================================================================
%% Eunit white box tests
%%====================================================================

% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ---------------------------------------------------------------------------------------------
with_iodevice() -> 
	put(io_devices, #{}),
	{ok, IoDevice} = file:open(?EUNIT_TEST_FILE, [write]),
	put(io, IoDevice).

delete_file(_) -> 
	file:delete(?EUNIT_TEST_FILE).


% ----------------------------------------------------------------------------------------------------------------------
% TESTS DESCRIPTIONS ---------------------------------------------------------------------------------------------------
make_report_test_() ->
    [{"Correct IoDevice addition, writting and deletion with internal functions",
      {setup, local, fun with_iodevice/0, fun delete_file/1,
	   {inorder, 
        [{"Correct IoDevice addition", ?_assert(add_iodevice_test())},
		 {"Correct IoDevice writting", ?_assert(write_iodevice_test())},
		 {"Correct IoDevice clossing", ?_assert(del_iodevice_test())},
		 {"io_devices is empty ", ?_assertMatch(#{}, get(io_devices))}]}}}].


% ----------------------------------------------------------------------------------------------------------------------
% ACTUAL TESTS ---------------------------------------------------------------------------------------------------------
add_iodevice_test() ->
	IoDevice = get(io),
	add_iodevice(IoDevice),
    lists:all(
		fun is_reference/1, 
		maps:keys(get(io_devices))).

write_iodevice_test() ->
	[Ref] =  maps:keys(get(io_devices)),
	EJSON = #{<<"foo">> => <<"bar">>},
	write_iodevice(Ref, EJSON),
	true.

del_iodevice_test() ->
	[Ref] =  maps:keys(get(io_devices)),
	ok == del_iodevice(Ref).


% ----------------------------------------------------------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS --------------------------------------------------------------------------------------------





