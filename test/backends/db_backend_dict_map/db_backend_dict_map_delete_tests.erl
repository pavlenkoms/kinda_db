-module(db_backend_dict_map_delete_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DICT_KEY, {db, databases}).
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
  erase(?DICT_KEY).

stop(_) ->
  erase(?DICT_KEY).

create_test_() ->
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

  Result = db_backend_dict_map:delete(Key1, DbName),

  #{DbName := Db} = get(?DICT_KEY),
  [?_assertEqual({ok, Record1}, Result), ?_assertEqual(#{Key2 => Record2}, Db)].

failure_for_missing_record(_) ->
  DbName = "TheName",
  Key2 = 2,
  Record2 = {Key2, "name2", "city2"},

  put(?DICT_KEY, #{DbName => #{Key2 => Record2}}),

  Result = db_backend_dict_map:delete(1, DbName),

  #{DbName := Db} = get(?DICT_KEY),
  [?_assertEqual({error, missing_record}, Result), ?_assertEqual(#{Key2 => Record2}, Db)].

failure_for_missing_db(_) ->
  put(?DICT_KEY, #{}),

  DbName = "TheName",
  Key = 1,
  Result = db_backend_dict_map:delete(Key, DbName),

  Dbs = get(?DICT_KEY),
  [?_assertEqual({error, missing_database}, Result), ?_assertEqual(#{}, Dbs)].

failure_for_uninitiated_dict(_) ->
  DbName = "TheName",
  Result = db_backend_dict_map:delete(1, DbName),

  Dbs = get(?DICT_KEY),
  [?_assertEqual({error, missing_database}, Result), ?_assertEqual(undefined, Dbs)].
