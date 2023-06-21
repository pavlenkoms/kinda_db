-module(db_backend_dict_map_create_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DICT_KEY, {db, databases}).
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
  erase(?DICT_KEY).

stop(_) ->
  erase(?DICT_KEY).

create_test_() ->
  [{"sucess for empty", ?setup(fun success/1)},
   {"failure for existing", ?setup(fun failure_for_existing/1)},
   {"failure for missing db", ?setup(fun failure_for_missing_db/1)},
   {"failure for uninitiated dict", ?setup(fun failure_for_uninitiated_dict/1)}].

success(_) ->
  DbName = "TheName",
  OldKey = 2,
  OldRecord = {OldKey, "name2", "city2"},

  put(?DICT_KEY, #{DbName => #{OldKey => OldRecord}}),

  Key = 1,
  Record = {Key, "name1", "city1"},
  Result = db_backend_dict_map:create(Record, DbName),

  #{DbName := Db} = get(?DICT_KEY),
  [?_assertEqual({ok, Record}, Result),
   ?_assertEqual(#{Key => Record, OldKey => OldRecord}, Db)].

failure_for_existing(_) ->
  DbName = "TheName",
  OldKey = 1,
  OldRecord = {OldKey, "name2", "city2"},

  put(?DICT_KEY, #{DbName => #{OldKey => OldRecord}}),

  Record = {OldKey, "name1", "city1"},
  Result = db_backend_dict_map:create(Record, DbName),

  #{DbName := Db} = get(?DICT_KEY),
  [?_assertEqual({error, value_already_exists}, Result),
   ?_assertEqual(#{OldKey => OldRecord}, Db)].

failure_for_missing_db(_) ->
  put(?DICT_KEY, #{}),

  DbName = "TheName",
  Key = 1,
  Record = {Key, "name1", "city1"},
  Result = db_backend_dict_map:create(Record, DbName),

  Dbs = get(?DICT_KEY),
  [?_assertEqual({error, missing_database}, Result), ?_assertEqual(#{}, Dbs)].

failure_for_uninitiated_dict(_) ->
  DbName = "TheName",
  Key = 1,
  Record = {Key, "name1", "city1"},
  Result = db_backend_dict_map:create(Record, DbName),

  Dbs = get(?DICT_KEY),
  [?_assertEqual({error, missing_database}, Result), ?_assertEqual(undefined, Dbs)].
