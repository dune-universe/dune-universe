let make_search_param ~tz_offset_s ~search_years_ahead ~from_unix_second =
  Daypack_lib.Search_param.make_using_years_ahead
    ~search_using_tz_offset_s:tz_offset_s ~start:(`Unix_second from_unix_second)
    search_years_ahead
