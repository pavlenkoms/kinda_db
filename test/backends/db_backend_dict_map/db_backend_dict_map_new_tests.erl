-module(db_backend_dict_map_new_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DICT_KEY, {db, databases}).
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
  erase(?DICT_KEY).

stop(_) ->
  erase(?DICT_KEY).

new_test_() ->
  [{"sucess for empty", ?setup(fun success_empty/1)},
   {"sucess for not empty dict", ?setup(fun success_not_empty_dict/1)},
   {"failure for existing db", ?setup(fun failure_for_existing_db/1)}].

success_empty(_) ->
  DbName = "TheName",
  Result = db_backend_dict_map:new(DbName),
  Dbs = get(?DICT_KEY),
  [?_assertEqual(ok, Result), ?_assertEqual(#{DbName => #{}}, Dbs)].

success_not_empty_dict(_) ->
  put(?DICT_KEY, #{"some_db" => #{}}),
  DbName = "TheName",
  Result = db_backend_dict_map:new(DbName),
  Dbs = get({db, databases}),
  [?_assertEqual(ok, Result), ?_assertEqual(#{DbName => #{}, "some_db" => #{}}, Dbs)].

failure_for_existing_db(_) ->
  DbName = "TheName",
  put(?DICT_KEY, #{DbName => #{}}),
  Result = db_backend_dict_map:new(DbName),
  [?_assertEqual({error, database_exists}, Result)].
