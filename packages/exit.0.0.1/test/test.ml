;;
assert (Exit.success_code >= 0) ;
assert (Exit.failure_code >= 0) ;
assert (Exit.success_code <= 255) ;
assert (Exit.failure_code <= 255) ;
assert (Exit.success_code <> Exit.failure_code)
