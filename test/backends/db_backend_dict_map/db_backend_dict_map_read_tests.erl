-module(db_backend_dict_map_read_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DICT_KEY, {db, databases}).
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
  erase(?DICT_KEY).

stop(_) ->
  erase(?DICT_KEY).

read_test_() ->
  [{"sucess", ?setup(fun success/1)},
   {"failure for missing record", ?setup(fun failure_for_missing_record/1)},
   {"failure for missing db", ?setup(fun failure_for_missing_db/1)},
   {"failure for uninitiated dict", ?setup(fun failure_for_uninitiated_dict/1)}].

success(_) ->
  DbName = "TheName",
  Key1 = 1,
  Record1 = {Key1, "name1", "city1"},
  Key2 = 2,
  Record2 = {Key2, "name2", "city2"},

  put(?DICT_KEY, #{DbName => #{Key1 => Record1, Key2 => Record2}}),

  Result = db_backend_dict_map:read(Key1, DbName),

  [?_assertEqual({ok, Record1}, Result)].

failure_for_missing_record(_) ->
  DbName = "TheName",
  Key = 1,
  Record = {Key, "name1", "city1"},

  put(?DICT_KEY, #{DbName => #{Key => Record}}),

  Result = db_backend_dict_map:read(2, DbName),

  [?_assertEqual({error, missing_record}, Result)].

failure_for_missing_db(_) ->
  put(?DICT_KEY, #{}),

  Result = db_backend_dict_map:read(1, "TheName"),

  [?_assertEqual({error, missing_database}, Result)].

failure_for_uninitiated_dict(_) ->
  DbName = "TheName",
  Key = 1,
  Record = {Key, "name1", "city1"},
  Result = db_backend_dict_map:create(Record, DbName),

  Dbs = get(?DICT_KEY),
  [?_assertEqual({error, missing_database}, Result), ?_assertEqual(undefined, Dbs)].
