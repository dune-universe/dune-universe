exception Impossible(string);

type sortOrder = Asc | Desc;

let identity: 'a => 'a = input => input;

let reduce: ((list('a), int, 'a, 'b) => 'b, 'b, list('a)) => 'b =
  (fold_func, starting_accumulator, starting_list) => {
    switch (starting_list) {
      | [] => starting_accumulator;
      | [first_item, ...rest_of_list] => {
        let rec internal_fold_func: (list('a), int, 'a, 'b) => 'b =
          (input_list, idx, internal_first_item, output) => {
            let new_output = fold_func(starting_list, idx, internal_first_item, output);
            switch (input_list) {
              | [] => new_output;
              | [next_item, ...list_dropping_one] =>
                  internal_fold_func(list_dropping_one, idx + 1, next_item, new_output);
            }
          };
        internal_fold_func(rest_of_list, 0, first_item, starting_accumulator);
      };
    };
  };

let map: ((list('a), int, 'a) => 'b, list('a)) => list('b) =
  (map_func, starting_list) =>
    starting_list |> reduce((l, idx, item, acc) => [map_func(l, idx, item), ...acc], []) |> List.rev;

let filter: ((list('a), int, 'a) => bool, list('a)) => list('a) = (filter_func, starting_list) =>
    starting_list
    |> reduce((l, idx, item, acc) => {
        filter_func(l, idx, item) ? [item, ...acc] : acc;
      }, [])
    |> List.rev;

let reject: ((list('a), int, 'a) => bool, list('a)) => list('a) = (rejection_func, starting_list) =>
  starting_list |> filter((l, idx, item) => !rejection_func(l, idx, item));

let head = List.hd;

let first = head;

let last: list('a) => 'a = list => list |> List.rev |> head;

let nth: (int, list('a)) => 'a = (selected_idx, input_list) => List.nth(input_list, selected_idx);

let tail = List.tl;

let initial: list('a) => list('a) = list => list |> List.rev |> tail |> List.rev;

let flatten = List.flatten;

let concat = flatten;

let flatMap: ((list('a), int, 'a) => list('b), list('a)) => list('b) =
  (map_func, starting_list) => starting_list |> map(map_func) |> flatten;

let forEach: ((list('a), int, 'a) => bool, list('a)) => list('a) =
  (each_func, starting_list) => {
    switch (starting_list) {
      | [] => [];
      | [first_item, ...rest_of_list] => {
        let rec internal_rec_func: (list('a), int, 'a) => unit =
          (rest_of_list, idx, internal_first_item) => {
            if (!each_func(starting_list, idx, internal_first_item)) {
              ();
            } else {
              switch (rest_of_list) {
                | [] => ();
                | [next_item, ...list_dropping_one] => {
                      internal_rec_func(list_dropping_one, idx + 1, next_item);
                  }
              }
            }
          };
        let _unit = internal_rec_func(rest_of_list, 0, first_item);
        starting_list;
      };
    };
  };

let each = forEach;

let forEachRight: ((list('a), int, 'a) => bool, list('a)) => list('a) =
  (each_func, starting_list) => {
    let _reversed_list = starting_list |> List.rev |> forEach(each_func);
    starting_list;
  };

let eachRight = forEachRight;

let dropWhile: ((list('a), int, 'a) => bool, list('a)) => list('a) =
  (while_func, starting_list) => {
    switch (starting_list) {
      | [] => [];
      | [first_item, ...rest_of_list] => {
        let rec internal_rec_func: (list('a), int, 'a) => list('a) =
          (rest_of_list, idx, internal_first_item) => {
            let remaining_list = [internal_first_item, ...rest_of_list];
            if (!while_func(starting_list, idx, internal_first_item)) {
              remaining_list;
            } else {
              switch (rest_of_list) {
                | [] => [];
                | [next_item, ...list_dropping_one] => {
                      internal_rec_func(list_dropping_one, idx + 1, next_item);
                  }
              }
            }
          };
        internal_rec_func(rest_of_list, 0, first_item);
      };
    };
  };

let drop: (int, list('a)) => list('a) =
  (count_to_drop, starting_list) => {
    let drop_func = (_, idx, _) => idx < count_to_drop;
    starting_list |> dropWhile(drop_func);
  };

let dropRightWhile: ((list('a), int, 'a) => bool, list('a)) => list('a) =
  (while_func, starting_list) => {
    starting_list |> List.rev |> dropWhile(while_func) |> List.rev
  };

let dropRight: (int, list('a)) => list('a) =
  (count_to_drop, starting_list) => {
    starting_list |> List.rev |> drop(count_to_drop) |> List.rev
  };

let takeWhile: ((list('a), int, 'a) => bool, list('a)) => list('a) =
  (while_func, starting_list) => {
    switch (starting_list) {
      | [] => [];
      | [first_item, ...rest_of_list] => {
        let rec internal_rec_func: (list('a), int, 'a, list('a)) => list('a) =
          (rest_of_list, idx, internal_first_item, output_list) => {
            if (!while_func(starting_list, idx, internal_first_item)) {
              output_list;
            } else {
              let updated_output_list = [internal_first_item, ...output_list];
              switch (rest_of_list) {
                | [] => updated_output_list;
                | [next_item, ...list_dropping_one] => {
                      internal_rec_func(list_dropping_one, idx + 1, next_item, updated_output_list);
                  }
              }
            }
          };
        internal_rec_func(rest_of_list, 0, first_item, []) |> List.rev
      };
    };
  };

let take: (int, list('a)) => list('a) =
  (count_to_take, starting_list) => {
    let take_func = (_, idx, _) => idx < count_to_take;
    starting_list |> takeWhile(take_func);
  };

let takeRightWhile: ((list('a), int, 'a) => bool, list('a)) => list('a) =
  (while_func, starting_list) => {
    starting_list |> List.rev |> takeWhile(while_func) |> List.rev
  };

let takeRight: (int, list('a)) => list('a) =
  (count_to_take, starting_list) => {
    starting_list |> List.rev |> take(count_to_take) |> List.rev
  };

let slice: (int, int, list('a)) => list('a) =
  (start_idx, end_idx, starting_list) => {
    if (end_idx < 0) {
      []
    } else {
      take(end_idx - start_idx + 1, drop(start_idx, starting_list));
    }
  }

let chunk: (int, list('a)) => list(list('a)) =
  (chunk_size, starting_list) => {
    if (chunk_size < 1) {
      raise(Invalid_argument("chunk_size must be a positive integer!"));
    } else {
      let evenly_split_remainder = List.length(starting_list) mod chunk_size;
      let last_chunk_length = (evenly_split_remainder === 0) ? chunk_size : evenly_split_remainder;
  
      let (_, _, chunked_list) = starting_list
      |> List.rev
      |> List.fold_left(((current_chunk_count, current_list_length, current_chunked_list), next_item) => {
        let chunk_limit = (current_list_length <= 1) ? last_chunk_length : chunk_size;
        let next_current_chunk_count = (current_chunk_count + 1 === chunk_limit) ? 0 : current_chunk_count + 1;
        if (current_chunk_count === 0) {
          (next_current_chunk_count, current_list_length + 1, [[next_item], ...current_chunked_list])
        } else {
          switch (current_chunked_list) {
          | [first_sublist, ...other_sublists] =>
            (next_current_chunk_count, current_list_length, [[next_item, ...first_sublist], ...other_sublists])
          | [] => raise(Impossible("The first item will always start with a chunk_count of 0!"));
          };
        }
      }, (0, 0, []));
      chunked_list;
    }
  };

let differenceBy: ('a => 'b, list('a), list('a)) => list('a) =
  (comparison_func, comparison_list, starting_list) => {
    let rec simple_some: ('a => bool, list('a)) => bool =
      (test_func, list_to_test) => {
        switch (list_to_test) {
        | [] => false;
        | [next_item, ...rest_of_list] => {
            if (test_func(next_item)) {
              true;
            } else {
              simple_some(test_func, rest_of_list);
            }
          }
        };
      };

    List.filter(item => {
      !(comparison_list |> simple_some(comparison_item => comparison_func(comparison_item) == comparison_func(item)))
    }, starting_list);
  };

let difference: (list('a), list('a)) => list('a) =
  (comparison_list, starting_list) =>
    starting_list |> differenceBy(identity, comparison_list);

let fill = (~start_index: int=0, ~end_index: option(int)=?, replacement: 'a, starting_list: list('a)) : list('a) => {
    switch (starting_list) {
      | [] => [];
      | [first_item, ...list_excluding_first] => {
        let defaulted_end_index = switch (end_index) {
        | None => List.length(starting_list) + 1
        | Some(idx) => idx
        };
        
        let rec internal_fill: (list('a), int, 'a, list('a)) => list('a) =
          (return_list, idx, next_item, rest_of_list) => {
            let next_return_item = (idx >= start_index && idx < defaulted_end_index) ? replacement: next_item;
            let updated_return_list = [next_return_item, ...return_list];
            switch (rest_of_list) {
              | [] => updated_return_list;
              | [updated_next_item, ...updated_rest_of_list] => {
                  internal_fill(updated_return_list, idx + 1, updated_next_item, updated_rest_of_list);
                }
            }
          }
        internal_fill([], 0, first_item, list_excluding_first) |> List.rev;
      }
    }
  };

let find: ((list('a), int, 'a) => bool, list('a)) => option('a) =
  (find_func, starting_list) => {
    switch (starting_list) {
      | [] => None;
      | [first_item, ...rest_of_list] => {
        let rec internal_find_func: (list('a), int, 'a) => option('a) =
          (rest_of_list, idx, internal_first_item) => {
            if (find_func(starting_list, idx, internal_first_item)) {
              Some(internal_first_item);
            } else {
              switch (rest_of_list) {
                | [] => None;
                | [next_item, ...list_dropping_one] => {
                      internal_find_func(list_dropping_one, idx + 1, next_item);
                  }
              }
            }
          };
        internal_find_func(rest_of_list, 0, first_item);
      };
    };
  };

let findLast: ((list('a), int, 'a) => bool, list('a)) => option('a) =
  (find_func, starting_list) => starting_list |> List.rev |> find(find_func);

let findIndex: ((list('a), int, 'a) => bool, list('a)) => int =
  (find_func, starting_list) => {
    switch (starting_list) {
      | [] => -1;
      | [first_item, ...rest_of_list] => {
        let rec internal_find_func: (list('a), int, 'a) => int =
          (rest_of_list, idx, internal_first_item) => {
            if (find_func(starting_list, idx, internal_first_item)) {
              idx;
            } else {
              switch (rest_of_list) {
                | [] => -1;
                | [next_item, ...list_dropping_one] => {
                      internal_find_func(list_dropping_one, idx + 1, next_item);
                  }
              }
            }
          };
        internal_find_func(rest_of_list, 0, first_item);
      };
    };
  };

let findLastIndex: ((list('a), int, 'a) => bool, list('a)) => int =
  (find_func, starting_list) => {
    let reverse_index = starting_list |> List.rev |> findIndex(find_func);
    (List.length(starting_list) - 1) - reverse_index;
  };

let some: ((list('a), int, 'a) => bool, list('a)) => bool =
  (some_func, starting_list) => {
    switch(starting_list |> find(some_func)) {
      | Some(_) => true;
      | None => false;
    };
  };

let every: ((list('a), int, 'a) => bool, list('a)) => bool =
  (every_func, starting_list) =>
    starting_list |> reduce((l, idx, item, acc) => every_func(l, idx, item) ? acc : false, true);

let includesBy: (('a, 'b) => bool, 'a, list('a)) => bool =
  (comparison_func, searched_item, starting_list) => {
    let find_func = (_, _, list_item) => comparison_func(list_item, searched_item);
    switch(starting_list |> find(find_func)) {
      | Some(_) => true;
      | None => false;
    };
  };

let includes: ('a, list('a)) => bool =
  (searched_item, starting_list) => {
    let items_are_same = (list_item, searched_item) => list_item == searched_item;
    starting_list |> includesBy(items_are_same, searched_item);
  };

let partition: ('a => bool, list('a)) => (list('a), list('a)) =
  (partition_func, starting_list) => {
    let rec internal_part_func: (list('a), list('a), list('a)) => (list('a), list('a)) =
      (list_to_test, true_list, false_list) => {
        switch (list_to_test) {
          | [] => (true_list, false_list);
          | [next_item, ...rest_of_list] => {
            let true_for_next_item = partition_func(next_item);
            let updated_true_list = true_for_next_item ? [next_item, ...true_list] : true_list;
            let updated_false_list = true_for_next_item ? false_list : [next_item, ...false_list];
            internal_part_func(rest_of_list, updated_true_list, updated_false_list);
          };
        };
      };

    let (true_list, false_list) = internal_part_func(starting_list, [], []);
    (List.rev(true_list), List.rev(false_list))
  };

let sampleSize: (int, list('a)) => list('a) =
  (sample_size, input_list) => {
    let input_list_size = List.length(input_list);

    let rec find_random_indices: (list(int)) => list(int) =
      (current_idx_list) => {
        let current_length = List.length(current_idx_list);
        if ((current_length == sample_size) || (current_length == input_list_size)) {
          current_idx_list;
        } else {
          let next_idx = Random.int(input_list_size);
          if (includes(next_idx, current_idx_list)) {
            find_random_indices(current_idx_list)
          } else {
            find_random_indices([next_idx, ...current_idx_list])
          }
        }
      };

    let random_indices = find_random_indices([]);

    let (elements_with_index_list, _) = input_list
      |> List.fold_left(((pairs, idx), next_item) => {
        if (includes(next_item, random_indices)) {
          ([(next_item, idx), ...pairs], idx + 1);
        } else {
          (pairs, idx + 1);
        }
      }, ([], 0));

    elements_with_index_list
      |> List.fast_sort(((_current_item, current_item_idx), (_next_item, next_item_idx)) => {
        current_item_idx - next_item_idx;
      })
      |> List.map(((item, _idx)) => item);
  };

let sample: list('a) => 'a = input_list => {
    if (List.length(input_list) < 1) {
      raise(Invalid_argument("input_list cannot be empty!"));
    };
    input_list |> sampleSize(1) |> head;
  };

let shuffle: list('a) => list('a) = input_list => input_list |> sampleSize(List.length(input_list));

let size = List.length;

let simpleSortBy: ('a => int, list('a)) => list('a) =
  (sort_func, starting_list) => {
    let comparison_func = (current_item, next_item) => sort_func(current_item) - sort_func(next_item);
    starting_list |> List.stable_sort(comparison_func)
  }

let sortBy: (list('a => int), list('a)) => list('a) =
  (sort_funcs, starting_list) => {
    let lists_by_desc_pref = List.rev(sort_funcs);
    switch (lists_by_desc_pref) {
      | [] => starting_list;
      | [first_sort_func, ...rest_of_sort_funcs] => {

        let rec internal_sort_func: (list('a => int), list('a), 'a => int) => list('a) =
          (remaining_sort_funcs, current_list, next_sort_func) => {
            let sorted_list = simpleSortBy(next_sort_func, current_list)
            switch (remaining_sort_funcs) {
              | [] => sorted_list;
              | [new_next_sort_func, ...new_remaining_sort_funcs] => {
                  internal_sort_func(new_remaining_sort_funcs, sorted_list, new_next_sort_func);
                }
            }
          };

        internal_sort_func(rest_of_sort_funcs, starting_list, first_sort_func);
      };
    };
  };

let orderBy: (list('a => int), list(sortOrder), list('a)) => list('a) =
  (sort_funcs, sort_orders, starting_list) => {
    let lists_by_desc_pref = List.rev(sort_funcs);
    let orders_by_desc_pref = List.rev(sort_orders);

    let get_next_sort_order: list(sortOrder) => (sortOrder, list(sortOrder)) =
      sort_orders => {
        switch (sort_orders) {
          | [] => (Asc, []);
          | [head_sort_order, ...other_sort_orders] => (head_sort_order, other_sort_orders);
        };
      };

    switch (lists_by_desc_pref) {
      | [] => starting_list;
      | [first_sort_func, ...rest_of_sort_funcs] => {

        let rec internal_sort_func: (list('a => int), list(sortOrder), list('a), 'a => int, sortOrder) => list('a) =
          (remaining_sort_funcs, remaining_sort_orders, current_list, next_sort_func, next_sort_order) => {
            let sorted_list = if (next_sort_order == Asc) {
              simpleSortBy(next_sort_func, current_list);
            } else {
              let desc_sort_func = i => next_sort_func(i) * -1;
              simpleSortBy(desc_sort_func, current_list);
            }
            switch (remaining_sort_funcs) {
              | [] => sorted_list;
              | [new_next_sort_func, ...new_remaining_sort_funcs] => {
                  let (new_next_sort_order, new_remaining_sort_orders) = get_next_sort_order(remaining_sort_orders)
                  internal_sort_func(new_remaining_sort_funcs, new_remaining_sort_orders, sorted_list, new_next_sort_func, new_next_sort_order);
                }
            }
          };

        let (first_sort_order, rest_of_sort_orders) = get_next_sort_order(orders_by_desc_pref);
        internal_sort_func(rest_of_sort_funcs, rest_of_sort_orders, starting_list, first_sort_func, first_sort_order);
      };
    };
  };

let join: (string, list(string)) => string = (join_str, starting_list) =>
  starting_list |> reduce((_, _, item, acc) => String.length(acc) > 0 ? (acc ++ join_str ++ item) : item, "");

let uniqWith: (('a, 'a) => bool, list('a)) => list('a) =
  (compare_func, starting_list) => {
    starting_list
    |> reduce((_, _, item, acc) => {
      acc |> includesBy(compare_func, item) ? acc : [item, ...acc];
    }, [])
    |> List.rev;
  };

let uniqBy: ('a => 'b, list('a)) => list('a) = (transform_func, starting_list) =>
  starting_list |> uniqWith((earlier_item, later_item) => transform_func(earlier_item) == transform_func(later_item));

let uniq: list('a) => list('a) = starting_list => starting_list |> uniqBy(identity);

let intersectionWith: (('a, 'a) => bool, list(list('a))) => list('a) =
  (compare_func, list_of_lists) => {
    switch (list_of_lists) {
      | [] => [];
      | [first_list, ...other_lists] => {
          first_list |> filter((_, _, first_list_item) => {
            other_lists |> every((_, _, other_list) => {
              other_list |> some((_, _, other_list_item) => compare_func(first_list_item, other_list_item));
            });
          });
        };
    };
  };

let intersectionBy: ('a => 'b, list(list('a))) => list('a) = (transform_func, list_of_lists) =>
  list_of_lists |> intersectionWith((earlier_item, later_item) => transform_func(earlier_item) == transform_func(later_item));

let intersection: list(list('a)) => list('a) = list_of_lists => list_of_lists |> intersectionBy(identity);

let xorWith: (('a, 'a) => bool, list(list('a))) => list('a) =
  (compare_func, list_of_lists) => {
    
    let list_of_sets = list_of_lists |> List.map(uniqWith(compare_func));

    let exclude_index = (index_to_exclude, starting_list) => starting_list |> reject((_, idx, _) => idx == index_to_exclude);

    let find_difference: (list(list('a)), list('a)) => list('a) = (other_lists, focused_list) => {
      focused_list |> reject((_, _, first_list_item) => {
        other_lists |> some((_, _, other_list) => {
          other_list |> some((_, _, other_list_item) => compare_func(first_list_item, other_list_item));
        });
      });
    };

    let rec internal_xor_with: (int, list(list('a)), list('a), list(list('a))) => list('a) =
      (index, difference_lists, current_set, remaining_sets) => {
        let noncurrent_sets = list_of_sets |> exclude_index(index);
        let current_difference_list = current_set |> find_difference(noncurrent_sets);
        let updated_difference_lists = [current_difference_list, ...difference_lists];
        switch (remaining_sets) {
          | [] => updated_difference_lists |> List.rev |> flatten |> uniqWith(compare_func);
          | [next_set, ...new_remaining_sets] =>
            internal_xor_with(index + 1, updated_difference_lists, next_set, new_remaining_sets);
        };
      };

    switch (list_of_sets) {
      | [] => [];
      | [first_set, ...other_sets] => internal_xor_with(0, [], first_set, other_sets);
    };
  };


let xorBy: ('a => 'b, list(list('a))) => list('a) = (transform_func, list_of_lists) =>
  list_of_lists |> xorWith((earlier_item, later_item) => transform_func(earlier_item) == transform_func(later_item));

let xor: list(list('a)) => list('a) = list_of_lists => list_of_lists |> xorBy(identity);

let unionWith: (('a, 'a) => bool, list(list('a))) => list('a) = (compare_func, list_of_lists) =>
  list_of_lists |> flatten |> uniqWith(compare_func);

let unionBy: ('a => 'b, list(list('a))) => list('a) = (transform_func, list_of_lists) =>
  list_of_lists |> unionWith((earlier_item, later_item) => transform_func(earlier_item) == transform_func(later_item));

let union: list(list('a)) => list('a) = list_of_lists => list_of_lists |> unionBy(identity);
 
let without: (list('a), list('a)) => list('a) = (exclusion_list, starting_list) =>
  starting_list |> filter((_, _, item) => !(exclusion_list |> includes(item)));

let zip: list(list('a)) => list(list('a)) = lists_to_zip => {
  switch (lists_to_zip) {
    | [] => [];
    | [first_list, ...other_lists] => {
      let first_list_length = List.length(first_list);
      let lists_are_equal_size = other_lists |> every((_, _, list) => List.length(list) == first_list_length);

      if (lists_are_equal_size) {
        let rec create_empty_lists: (list(list('a)), int) => list(list('a)) = (skeleton, idx) =>
          (idx >= first_list_length) ? skeleton : create_empty_lists([[], ...skeleton], idx + 1);

        lists_to_zip
        |> List.fold_left(
          (current_zipped_lists, next_list) =>
            List.map2((item, zipped_list) => [item, ...zipped_list], next_list, current_zipped_lists),
          create_empty_lists([], 0)
        )
        |> List.map(List.rev);

      } else {
        raise(Invalid_argument("lists_to_zip must all be of equal size!"));
      };
    };
  };
};

let unzip = zip;