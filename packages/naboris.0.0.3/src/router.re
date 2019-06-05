exception InvalidUrl(string);
exception DuplicateRoute(string);

open Query;

let getRawQuery = uri =>
  switch (Uri.verbatim_query(uri)) {
  | None => ""
  | Some(rawQuery) => rawQuery
  };

let addQuery = (queries, (key, value)) => QueryMap.add(key, value, queries);

let getQuery = uri =>
  switch (Uri.verbatim_query(uri)) {
  | Some(encodedQueryStr) =>
    List.fold_left(
      (queries, (key, value)) => addQuery(queries, (key, value)),
      QueryMap.empty,
      Uri.query_of_encoded(encodedQueryStr),
    )
  | None => QueryMap.empty
  };

let processPath = target => {
  let uri = Uri.of_string(target);
  switch (String.split_on_char('/', Uri.path(uri))) {
  | [_, ...path] => (
      List.map(Uri.pct_decode, path),
      getRawQuery(uri),
      getQuery(uri),
    )
  | _ => raise(InvalidUrl(target))
  };
};

let generateRoute = (target, method) => {
  let (path, rawQuery, query) = processPath(target);
  Route.{path, rawQuery, query, method};
};