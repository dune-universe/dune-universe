let search:
  (
    ~get: 'key => 'value,
    ~getMiddle: ('key, 'key) => 'key,
    ~continue: ('key, 'key) => bool,
    ~compare: ('value, 'value) => int,
    ~testCompare: 'value => int,
    ~first: 'key,
    ~last: 'key,
    unit
  ) =>
  option('key);
