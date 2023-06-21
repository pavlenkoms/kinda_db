-module(db_backend_dict_map).

-behaviour(db_backend).

-export([new/1, create/2, read/2, update/2, delete/2]).

-define(DICT_KEY, {db, databases}).

new(DbName) ->
  case get(?DICT_KEY) of
    #{DbName := _} ->
      {error, database_exists};
    undefined ->
      put(?DICT_KEY, #{DbName => #{}}),
      ok;
    Dbs ->
      put(?DICT_KEY, Dbs#{DbName => #{}}),
      ok
  end.

create({Key, UserName, City} = Record, DbName)
  when is_integer(Key), is_list(UserName), is_list(City) ->
  case get(?DICT_KEY) of
    #{DbName := #{Key := _}} ->
      {error, value_already_exists};
    #{DbName := Db} = Dbs ->
      Db1 = Db#{Key => Record},
      put(?DICT_KEY, Dbs#{DbName => Db1}),
      {ok, Record};
    #{} ->
      {error, missing_database};
    undefined ->
      {error, missing_database};
    _ ->
      {error, unexpected_error}
  end;
create(_Record, _DbName) ->
  {error, bad_record}.

read(Key, DbName) when is_integer(Key) ->
  case get(?DICT_KEY) of
    #{DbName := #{Key := Record}} ->
      {ok, Record};
    #{DbName := _} ->
      {error, missing_record};
    #{} ->
      {error, missing_database};
    undefined ->
      {error, missing_database};
    _ ->
      {error, unexpected_error}
  end;
read(_Key, _DbName) ->
  {error, bad_key}.

update({Key, UserName, City} = Record, DbName)
  when is_integer(Key), is_list(UserName), is_list(City) ->
  case get(?DICT_KEY) of
    #{DbName := #{Key := _} = Db} = Dbs ->
      Db1 = Db#{Key => Record},
      put(?DICT_KEY, Dbs#{DbName => Db1}),
      {ok, Record};
    #{DbName := _} ->
      {error, missing_record};
    #{} ->
      {error, missing_database};
    undefined ->
      {error, missing_database};
    _ ->
      {error, unexpected_error}
  end;
update(_Record, _DbName) ->
  {error, bad_record}.

delete(Key, DbName) when is_integer(Key) ->
  case get(?DICT_KEY) of
    #{DbName := #{Key := Record} = Db} = Dbs ->
      Db1 = maps:remove(Key, Db),
      put(?DICT_KEY, Dbs#{DbName => Db1}),
      {ok, Record};
    #{DbName := _} ->
      {error, missing_record};
    #{} ->
      {error, missing_database};
    undefined ->
      {error, missing_database};
    _ ->
      {error, unexpected_error}
  end;
delete(_Key, _DbName) ->
  {error, bad_key}.
