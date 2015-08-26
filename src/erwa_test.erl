%% @author ethrbh
%% @doc @todo Add description to erwa_test.


-module(erwa_test).

%% ====================================================================
%% Dummy MNESIA table
%% ====================================================================
-record(rDummyTableRec, {
							id,
							data
  						}).

-define(DUMMY_TABLE_RECORD_FIELDS, record_info(fields, rDummyTableRec)).
-define(DUMMY_TABLE_RECORD_MNESIA_TABLE, dummy_mnesia_table).
-define(DUMMY_TABLE_RECORD_MNESIA_TABLE_ATTRIBUTES,
    [?DUMMY_TABLE_RECORD_MNESIA_TABLE,
    [{attributes, record_info(fields,rDummyTableRec)},
     {record_name,rDummyTableRec},
     {disc_copies,[node()]},
     {type,set}]]).

-define(MNESIA_TABLE_LIST, [
							?DUMMY_TABLE_RECORD_MNESIA_TABLE_ATTRIBUTES
							]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0]).

init() ->
	%% Check MNESIA tables. Halt ErlangVM is no local_tables found.
	case mnesia:system_info(local_tables) of
		[] ->
			error_logger:info_report(["No local MNESIA tables are have been found"]),
			
			init_mnesia_tables(),
			
			error_logger:info_report(["Halt Erlang VM"]),
			erlang:halt();
		T ->
			case lists:member(?DUMMY_TABLE_RECORD_MNESIA_TABLE, T) of
				true ->
					error_logger:info_report(["Local MNESIA tables have been found", {tables, T}]);
				false ->
					error_logger:info_report(["Local MNESIA tables have been found, but dummy table is missing", {tables, T}]),
					
					init_mnesia_tables(),
					
					error_logger:info_report(["Halt Erlang VM"]),
					erlang:halt()
			end
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% Init MNESIA tables
%% ====================================================================
init_mnesia_tables() ->
	error_logger:info_report(["Stop MNESIA"]),
	mnesia:stop(),
	
	error_logger:info_report(["Create MNESIA schema"]),
	mnesia:create_schema([node()]),
	
	error_logger:info_report(["Re-start MNESIA"]),
	mnesia:start(),
	
	error_logger:info_report(["Create MNESIA tables"]),
	create_all_mnesia_table(?MNESIA_TABLE_LIST),
	
	waiting_for_mnesia_tables(?MNESIA_TABLE_LIST, 500, 5, do_crash_when_failed),
	ok.

%% ====================================================================
%% Create all pre-defined mnesia table.
%% Input:
%%      TableList   -   list of MNESIA tables
%% Output:
%%      0
%% ====================================================================
create_all_mnesia_table(TableList)->
    create_all_mnesia_table_loop(TableList,[]).
    
create_all_mnesia_table_loop([],Result)->
    Result;
create_all_mnesia_table_loop([TableInfo | Tail],Result)->
    [TableName,AttributeList] = TableInfo,
    R = mnesia:create_table(TableName,AttributeList),
	error_logger:info_report(["Create MNESIA table", 
							  {table, TableName},
							  {attributes, AttributeList},
							  {result, R}]),
    create_all_mnesia_table_loop(Tail,Result++[R]).

%% ====================================================================
%% Waits until all MNESIA tables are created
%% Input:
%%      Tables  				-   list of table definitions, see xxAXECommon/mnesia_table_list
%%      Timeout 				-   integer, timeout in miliseconds (for one try)
%%      Retries 				-   integer, retries
%%      ShouldDoCrashWhenFailed	-	atom, do_crash_when_failed | do_not_crash_when_failed
%% Output:
%%      Tables  -   list of atom
%% ====================================================================
waiting_for_mnesia_tables(Tables,Timeout,Retries,ShouldDoCrashWhenFailed) when is_list(Tables), is_integer(Timeout), is_integer(Retries) ->
    Fun = fun({Tab,_KeyPos}) -> Tab;
             ([Tab,_Attr]) -> Tab;
             (Tab) when is_atom(Tab) -> Tab
          end,
    Tabs = lists:map(Fun,Tables),
	
	error_logger:info_report(["Waiting for MNESIA tables..."]), 
	
    waiting_for_mnesia_tables_loop(Tabs,Timeout,Retries,ShouldDoCrashWhenFailed).

waiting_for_mnesia_tables_loop(Tabs,Timeout,Retries,ShouldDoCrashWhenFailed) ->

    %% crash if not successful
    case mnesia:wait_for_tables(Tabs,Timeout) of
        ok ->
            Tabs;
        {timeout,BadTabList} ->
            case Retries of
                0 ->
                    error_logger:error_report(["Mnesia table start timeout no more retries, crashing myself intentionally.",
                                               {timeout,Timeout},
                                               {not_started_tables,BadTabList}]),
                    case ShouldDoCrashWhenFailed of
						do_crash_when_failed ->
							%% crash intentionally
							error_logger:error_report(["Do crash intentionally"]),
		                    ok = Retries;
						_->	%% No crash, but returns with empty list, what means no tables are started.
							[]
					end;
                _ ->
                    error_logger:info_report(["Mnesia table start timeout, retrying...",
                                              {timeout,Timeout},
                                              {not_started_tables,BadTabList},
                                              {remaining_retries,Retries}]),
                    waiting_for_mnesia_tables_loop(Tabs,Timeout,Retries - 1,ShouldDoCrashWhenFailed)
            end;
        Other ->
            error_logger:error_report(["Mnesia table start problem, crashing myself intentionally.",
                                      {timeout,Timeout},
                                      {reason,Other}]),
            case ShouldDoCrashWhenFailed of
				do_crash_when_failed ->
					%% crash intentionally
					error_logger:error_report(["Do crash intentionally"]),
                    ok = Retries;
				_->	%% No crash, but returns with empty list, what means no tables are started.
					[]
			end
    end.
