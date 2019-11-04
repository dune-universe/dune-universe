open OUnit;
open Lib;

type dog = {
  name: string,
  weight: int
};

let suite =
  "Odash" >::: [
    "identity returns its argument" >:: () => {
      let a_number = 1;
      let a_string = "hi";
      let a_list = [1,4];
      a_number |> Odash.identity |> assert_equal(a_number);
      a_string |> Odash.identity |> assert_equal(a_string);
      a_list |> Odash.identity |> assert_equal(a_list);
    },
    "reduce applies a function to each member of a list and an accumulator" >:: () => {
      let input_list = [1,2,3,4];
      let fold_func = (_, _, i, acc) => i + acc;
      let accumulator = 0;
      let expected_output = accumulator + 1 + 2 + 3 + 4;
      input_list |> Odash.reduce(fold_func, accumulator) |> assert_equal(expected_output);
    },
    "map can apply a function using starting list passed as first argument to map_func" >:: () => {
      let input_list = [1,2,3,4];
      let fold_func = (l, _, i, acc) => List.hd(l) + i + acc;
      let accumulator = 0;
      /* adds each element itself _and_ the first element for each */
      let expected_output = accumulator + 1 + 1 + 1 + 2 + 1 + 3 + 1 + 4;
      input_list |> Odash.reduce(fold_func, accumulator) |> assert_equal(expected_output);
    },
    "reduce can apply a function using item index passed as second argument to map_func" >:: () => {
      let input_list = [1,2,3,4];
      let fold_func = (_, idx, item, acc) => idx + item + acc;
      let accumulator = 0;
      /* adds each element itself _and_ the element's index for each */
      let expected_output = accumulator + 0 + 1 + 1 + 2 + 2 + 3 + 3 + 4;
      input_list |> Odash.reduce(fold_func, accumulator) |> assert_equal(expected_output);
    },
    "reduce returns the accumulator for an empty list" >:: () => {
      let input_list = [];
      let fold_func = (_, _, item, acc) => String.length(item) + acc;
      let accumulator = 0;
      input_list |> Odash.reduce(fold_func, accumulator) |> assert_equal(accumulator);
    },
    "map applies a function to each member of a list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let map_func = (_, _, i) => i + 1;
      let expected_output = [1,2,3,4,5,6,7,8,9,10,11,12];
      input_list |> Odash.map(map_func) |> assert_equal(expected_output);
    },
    "map can apply a function using starting list passed as first argument to map_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let map_func = (l, _, i) => Odash.includes(i + 5, l) ? i + 5 : i;
      let expected_output = [5,6,7,8,9,10,11,7,8,9,10,11];
      input_list |> Odash.map(map_func) |> assert_equal(expected_output);
    },
    "map can apply a function using item index passed as second argument to map_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let map_func = (_, idx, item) => idx + item;
      let expected_output = [0,2,4,6,8,10,12,14,16,18,20,22];
      input_list |> Odash.map(map_func) |> assert_equal(expected_output);
    },
    "map passes through an empty list unchanged" >:: () => {
      let input_list = [];
      let map_func = (_, idx, item) => idx + item;
      input_list |> Odash.map(map_func) |> assert_equal(input_list);
    },
    "filter filters items from a list that return false for filter_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let filter_func = (_, _, i) => (i mod 2) == 0;
      let expected_output = [0,2,4,6,8,10];
      input_list |> Odash.filter(filter_func) |> assert_equal(expected_output);
    },
    "filter returns an empty list if filter func always returns false" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let filter_func = (_, _, _) => false;
      let expected_output = [];
      input_list |> Odash.filter(filter_func) |> assert_equal(expected_output);
    },
    "filter returns input list if filter func always returns true" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let filter_func = (_, _, _) => true;
      input_list |> Odash.filter(filter_func) |> assert_equal(input_list);
    },
    "filter returns an empty input list unchanged" >:: () => {
      let input_list = [];
      let filter_func = (_, _, _) => false;
      input_list |> Odash.filter(filter_func) |> assert_equal(input_list);
    },
    "reject rejects items from a list that return true for reject_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let reject_func = (_, _, i) => (i mod 2) == 0;
      let expected_output = [1,3,5,7,9,11];
      input_list |> Odash.reject(reject_func) |> assert_equal(expected_output);
    },
    "reject returns an empty list if reject func always returns true" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let reject_func = (_, _, _) => true;
      let expected_output = [];
      input_list |> Odash.reject(reject_func) |> assert_equal(expected_output);
    },
    "reject returns input list if reject func always returns false" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let reject_func = (_, _, _) => false;
      input_list |> Odash.reject(reject_func) |> assert_equal(input_list);
    },
    "reject returns an empty input list unchanged" >:: () => {
      let input_list = [];
      let reject_func = (_, _, _) => true;
      input_list |> Odash.reject(reject_func) |> assert_equal(input_list);
    },
    "head returns the list head" >:: () => {
      let input_list = [4,5,6,7,8,9];
      let expected_output = 4;
      input_list |> Odash.head |> assert_equal(expected_output);
    },
    "first aliases head" >:: () => {
      let input_list = [4,5,6,7,8,9];
      input_list |> Odash.first |> assert_equal(Odash.head(input_list));
    },
    "last returns the last element in the input_list" >:: () => {
      let input_list = [4,5,6,7,8,9];
      let expected_output = 9;
      input_list |> Odash.last |> assert_equal(expected_output);
    },
    "nth returns the nth element in the input_list" >:: () => {
      let input_list = [4,5,6,7,8,9];
      let selected_index = 3;
      let expected_output = 7;
      input_list |> Odash.nth(selected_index) |> assert_equal(expected_output);
    },
    "tail returns the list tail" >:: () => {
      let input_list = [4,5,6,7,8,9];
      let expected_output = [5,6,7,8,9];
      input_list |> Odash.tail |> assert_equal(expected_output);
    },
    "initial returns the list excluding the last element" >:: () => {
      let input_list = [4,5,6,7,8,9];
      let expected_output = [4,5,6,7,8];
      input_list |> Odash.initial |> assert_equal(expected_output);
    },
    "flatten flattens a list of lists (by one level of depth)" >:: () => {
      let input_list = [[0,1],[2],[3],[4,5],[6,7,8,9],[10,11]];
      let expected_output = [0,1,2,3,4,5,6,7,8,9,10,11];
      input_list |> Odash.flatten |> assert_equal(expected_output);
    },
    "flatten passes through an empty list unchanged" >:: () => {
      let input_list = [];
      input_list |> Odash.flatten |> assert_equal(input_list);
    },
    "concat aliases flatten" >:: () => {
      let input_list = [[0,1],[2],[3],[4,5],[6,7,8,9],[10,11]];
      input_list |> Odash.concat |> assert_equal(Odash.flatten(input_list));
    },
    "flatMap applies a function then flattens the results" >:: () => {
      let input_list = [0,1,2,3,4];
      let map_func = (_, idx, item) => [idx, item];
      let expected_output = [0,0,1,1,2,2,3,3,4,4];
      input_list |> Odash.flatMap(map_func) |> assert_equal(expected_output);
    },
    "flatMap passes through an empty list unchanged" >:: () => {
      let input_list = [];
      let map_func = (_, idx, item) => [idx, item];
      input_list |> Odash.flatMap(map_func) |> assert_equal(input_list);
    },
    "forEach runs a function for its side effect then returns the initial list unchanged" >:: () => {
      let input_list = ['a','b','c','d'];
      let array_to_mutate = [|'b','b','b','b'|];
      let each_func = (_, idx, item) => {
        let _ = Array.set(array_to_mutate, idx, item);
        true;
      };
      let expected_mutated_array = [|'a','b','c','d'|];
      let _ = input_list |> Odash.forEach(each_func) |> assert_equal(input_list);
      assert_equal(array_to_mutate, expected_mutated_array)
    },
    "each aliases forEach" >:: () => {
      let for_each_input_list = ['a','b','c','d'];
      let for_each_array_to_mutate = [|'b','b','b','b'|];
      let for_each_func = (_, idx, item) => {
        let _ = Array.set(for_each_array_to_mutate, idx, item);
        true;
      };

      let each_input_list = ['a','b','c','d'];
      let each_array_to_mutate = [|'b','b','b','b'|];
      let each_func = (_, idx, item) => {
        let _ = Array.set(each_array_to_mutate, idx, item);
        true;
      };

      let _ = for_each_input_list |> Odash.forEach(for_each_func) |> assert_equal(for_each_input_list);
      let _ = each_input_list |> Odash.each(each_func) |> assert_equal(each_input_list);
      assert_equal(for_each_array_to_mutate, each_array_to_mutate)
    },
    "forEach exits early if the each_func returns false" >:: () => {
      let input_list = ['a','b','c','d'];
      let array_to_mutate = [|'b','b','b','b'|];
      let each_func = (_, idx, item) => {
        if (item == 'c') {
          false;
        } else {
          let _ = Array.set(array_to_mutate, idx, item);
          true;
        }
      };
      let expected_mutated_array = [|'a','b','b','b'|];
      let _ = input_list |> Odash.forEach(each_func) |> assert_equal(input_list);
      assert_equal(array_to_mutate, expected_mutated_array)
    },
    "forEach can use whole list within each_func" >:: () => {
      let input_list = ['a','b','c','d'];
      let array_to_mutate = [|'b','b','b','b'|];
      let each_func = (whole_list, _, _) => {
        whole_list |> List.iteri((idx, item) => Array.set(array_to_mutate, idx, item));
        false;
      };
      let expected_mutated_array = [|'a','b','c','d'|];
      let _ = input_list |> Odash.forEach(each_func) |> assert_equal(input_list);
      assert_equal(array_to_mutate, expected_mutated_array)
    },
    "forEach does nothing and returns empty list when passed an empty list" >:: () => {
      let input_list = [];
      let array_to_mutate = [|'b','b','b','b'|];
      let each_func = (_, idx, item) => {
        let _ = Array.set(array_to_mutate, idx, item);
        true;
      };
      let expected_mutated_array = [|'b','b','b','b'|];
      let _ = input_list |> Odash.forEach(each_func) |> assert_equal(input_list);
      assert_equal(array_to_mutate, expected_mutated_array)
    },
    "forEachRight runs a function for its side effect in reverse order and with inverse index, then returns the initial list unchanged" >:: () => {
      let input_list = ['a','b','c','d'];
      let array_to_mutate = [|'b','b','b','b'|];
      let each_func = (_, idx, item) => {
        let _ = Array.set(array_to_mutate, idx, item);
        true;
      };
      let expected_mutated_array = [|'d','c','b','a'|];
      let _ = input_list |> Odash.forEachRight(each_func) |> assert_equal(input_list);
      assert_equal(array_to_mutate, expected_mutated_array)
    },
    "eachRight aliases forEachRight" >:: () => {
      let for_each_input_list = ['a','b','c','d'];
      let for_each_array_to_mutate = [|'b','b','b','b'|];
      let for_each_func = (_, idx, item) => {
        let _ = Array.set(for_each_array_to_mutate, idx, item);
        true;
      };

      let each_input_list = ['a','b','c','d'];
      let each_array_to_mutate = [|'b','b','b','b'|];
      let each_func = (_, idx, item) => {
        let _ = Array.set(each_array_to_mutate, idx, item);
        true;
      };

      let _ = for_each_input_list |> Odash.forEachRight(for_each_func) |> assert_equal(for_each_input_list);
      let _ = each_input_list |> Odash.eachRight(each_func) |> assert_equal(each_input_list);
      assert_equal(for_each_array_to_mutate, each_array_to_mutate)
    },
    "chunk splits evenly divisible list into list of lists" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let chunk_size = 3;
      let expected_output = [[0,1,2],[3,4,5],[6,7,8],[9,10,11]];
      input_list |> Odash.chunk(chunk_size) |> assert_equal(expected_output);
    },
    "chunk splits not-evenly-divisible list with remainder in last sublist" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10];
      let chunk_size = 3;
      let expected_output = [[0,1,2],[3,4,5],[6,7,8],[9,10]];
      input_list |> Odash.chunk(chunk_size) |> assert_equal(expected_output);
    },
    /* TODO: figure out why this doesn't type check
    "chunk raises invalid if chunk_size is negative" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let chunk_size = -3;
      let expected_exception = Odash.Invalid("chunk_size must be a positive integer!");
      input_list |> Odash.chunk(chunk_size) |> assert_raises(expected_exception);
    }, */
    "dropWhile applies the provided func to the next item, index, and rest of list, and drops until false" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, i) => i < 5;
      let expected_output = [5,6,7,8,9,10,11];
      input_list |> Odash.dropWhile(while_func) |> assert_equal(expected_output);
    },
    "dropWhile can operate on item and full list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,3];
      let while_func = (list, _, next_item) => {
        list
        |> List.filter(i => i == next_item)
        |> List.length == 1;
      };
      let expected_output = [3,4,5,6,7,8,9,10,3];
      input_list |> Odash.dropWhile(while_func) |> assert_equal(expected_output);
    },
    "dropWhile returns list unaltered if provided func is false for the first item" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, _) => false;
      input_list |> Odash.dropWhile(while_func) |> assert_equal(input_list);
    },
    "dropWhile returns empty list if provided func is true for all items" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, _) => true;
      let expected_output = [];
      input_list |> Odash.dropWhile(while_func) |> assert_equal(expected_output);
    },
    "drop n returns a list with n items dropped from the beginning" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let drop_size = 3;
      let expected_output = [3,4,5,6,7,8,9,10,11];
      input_list |> Odash.drop(drop_size) |> assert_equal(expected_output);
    },
    "drop 0 returns a list as-is" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let drop_size = 0;
      input_list |> Odash.drop(drop_size) |> assert_equal(input_list);
    },
    "drop of a negative int returns a list as-is" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let drop_size = -99;
      input_list |> Odash.drop(drop_size) |> assert_equal(input_list);
    },
    "dropRightWhile applies the provided func to the last item, reverse index, and rest of list, and drops until false" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, i) => i > 5;
      let expected_output = [0,1,2,3,4,5];
      input_list |> Odash.dropRightWhile(while_func) |> assert_equal(expected_output);
    },
    "dropRight n returns a list with n items dropped from the end" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let drop_size = 3;
      let expected_output = [0,1,2,3,4,5,6,7,8];
      input_list |> Odash.dropRight(drop_size) |> assert_equal(expected_output);
    },
    "takeWhile applies the provided func to the next item, index, and rest of list, and keeps until false" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, i) => i < 5;
      let expected_output = [0,1,2,3,4];
      input_list |> Odash.takeWhile(while_func) |> assert_equal(expected_output);
    },
    "takeWhile can operate on item and full list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,7];
      let while_func = (list, _, next_item) => {
        list
        |> List.filter((i) => i == next_item)
        |> List.length == 1;
      };
      let expected_output = [0,1,2,3,4,5,6];
      input_list |> Odash.takeWhile(while_func) |> assert_equal(expected_output);
    },
    "takeWhile returns an empty list if provided func is false for the first item" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, _) => false;
      let expected_output = [];
      input_list |> Odash.takeWhile(while_func) |> assert_equal(expected_output);
    },
    "takeWhile a list unaltered if provided func is true for all items" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, _) => true;
      input_list |> Odash.takeWhile(while_func) |> assert_equal(input_list);
    },
    "take n returns a list with n items dropped from the beginning" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let take_size = 3;
      let expected_output = [0,1,2];
      input_list |> Odash.take(take_size) |> assert_equal(expected_output);
    },
    "take 0 returns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let take_size = 0;
      let expected_output = [];
      input_list |> Odash.take(take_size) |> assert_equal(expected_output);
    },
    "take of a negative int returns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let take_size = -99;
      let expected_output = [];
      input_list |> Odash.take(take_size) |> assert_equal(expected_output);
    },
    "takeRightWhile applies the provided func to the last item, reverse index, and rest of list, and takes until false" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, i) => i > 5;
      let expected_output = [6,7,8,9,10,11];
      input_list |> Odash.takeRightWhile(while_func) |> assert_equal(expected_output);
    },
    "takeRight n returns a list with n items taken from the end" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let take_size = 3;
      let expected_output = [9,10,11];
      input_list |> Odash.takeRight(take_size) |> assert_equal(expected_output);
    },
    "slice returns a sublist from start index to end index" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = 3;
      let end_idx = 7;
      let expected_output = [3,4,5,6,7];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "slice with same start and end returns a list of one" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = 3;
      let end_idx = 3;
      let expected_output = [3];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "slice with an end index before its start returns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = 3;
      let end_idx = 2;
      let expected_output = [];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "slice with a start index out of range returns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = 32;
      let end_idx = 35;
      let expected_output = [];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "slice with a negative start and end index returns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = -7;
      let end_idx = -2;
      let expected_output = [];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "slice with a negative end index but positive start indexreturns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = 7;
      let end_idx = -2;
      let expected_output = [];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "some returns true if one item meets criteria function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let test_func = (_, _, i) => i > 10;
      let expected_output = true;
      input_list |> Odash.some(test_func) |> assert_equal(expected_output);
    },
    "some returns true if all items meet criteria function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let test_func = (_, _, i) => i > -1;
      let expected_output = true;
      input_list |> Odash.some(test_func) |> assert_equal(expected_output);
    },
    "difference returns items in first list not in second" >:: () => {
      let starting_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let comparison_list = [0,2,4,6,8,10,12];
      let expected_output = [1,3,5,7,9,11];
      starting_list |> Odash.difference(comparison_list) |> assert_equal(expected_output);
    },
    "difference returns items in first list not in second, matching by ==" >:: () => {
      let starting_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let comparison_list = [0,2,4,6,8,10,12];
      let expected_output = [1,3,5,7,9,11];
      starting_list |> Odash.difference(comparison_list) |> assert_equal(expected_output);
    },
    "difference returns first list as-is if no matches in second list" >:: () => {
      let starting_list = [0.0,1.1,2.2,3.3,4.4];
      let comparison_list = [6.6,8.8,10.10,12.12];
      starting_list |> Odash.difference(comparison_list) |> assert_equal(starting_list);
    },
    "difference returns empty list if all match with second list" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let comparison_list = ['a','b','c','d','e','f'];
      let expected_output = [];
      starting_list |> Odash.difference(comparison_list) |> assert_equal(expected_output);
    },
    "differenceBy returns items in first list not in second, matching by comparison_func" >:: () => {
      let starting_list = [-1,3,-4,-7];
      let comparison_list = [1,-2,-4];
      let comparison_func = n => abs(n)
      let expected_output = [3,-7];
      starting_list |> Odash.differenceBy(comparison_func, comparison_list) |> assert_equal(expected_output);
    },
    "fill fills in a list with the provided value" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let expected_output = ['f','f','f','f','f'];
      starting_list |> Odash.fill(fill_character) |> assert_equal(expected_output);
    },
    "fill can take an inclusive start index" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let start_idx = 2;
      let expected_output = ['a','b','f','f','f'];
      starting_list |> Odash.fill(~start_index=start_idx, fill_character) |> assert_equal(expected_output);
    },
    "fill replaces all list values if start index is zero" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let start_idx = 0;
      let expected_output = ['f','f','f','f','f'];
      starting_list |> Odash.fill(~start_index=start_idx, fill_character) |> assert_equal(expected_output);
    },
    "fill replaces all list values if start index is negative" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let start_idx = -100;
      let expected_output = ['f','f','f','f','f'];
      starting_list |> Odash.fill(~start_index=start_idx, fill_character) |> assert_equal(expected_output);
    },
    "fill doesn't replace any list values if start index is greater than or equal to list size" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let start_idx = 5;
      starting_list |> Odash.fill(~start_index=start_idx, fill_character) |> assert_equal(starting_list);
    },
    "fill can take an exclusive end index" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let end_idx = 2;
      let expected_output = ['f','f','c','d','e'];
      starting_list |> Odash.fill(~end_index=end_idx, fill_character) |> assert_equal(expected_output);
    },
    "fill doesn't replace any list values if end index is zero" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let end_idx = 0;
      starting_list |> Odash.fill(~end_index=end_idx, fill_character) |> assert_equal(starting_list);
    },
    "fill doesn't replace any list values if end index is negative" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let end_idx = -100;
      starting_list |> Odash.fill(~end_index=end_idx, fill_character) |> assert_equal(starting_list);
    },
    "fill replaces all list values if end index is greater than list size" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let end_idx = 6;
      let expected_output = ['f','f','f','f','f'];
      starting_list |> Odash.fill(~end_index=end_idx, fill_character) |> assert_equal(expected_output);
    },
    "fill can take both a start and an end index together" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let expected_output = ['a','b','f','f','e'];
      starting_list |> Odash.fill(~start_index=2, ~end_index=4, fill_character) |> assert_equal(expected_output);
    },
    "find returns Some of first element in a list that returns true for the find_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let find_func = (_, _, i) => i > 5;
      let expected_output = Some(6);
      input_list |> Odash.find(find_func) |> assert_equal(expected_output);
    },
    "find returns None for a list in which no element returns true for the find_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let find_func = (_, _, i) => i > 50;
      let expected_output = None;
      input_list |> Odash.find(find_func) |> assert_equal(expected_output);
    },
    "find returns Some of first element in a list if all elements return true for the find_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let find_func = (_, _, _) => true;
      let expected_output = Some(0);
      input_list |> Odash.find(find_func) |> assert_equal(expected_output);
    },
    "find returns None for an empty list" >:: () => {
      let input_list = [];
      let find_func = (_, _, _) => true;
      let expected_output = None;
      input_list |> Odash.find(find_func) |> assert_equal(expected_output);
    },
    "findLast returns Some of last element in a list that returns true for the find_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let find_func = (_, _, i) => i < 5;
      let expected_output = Some(4);
      input_list |> Odash.findLast(find_func) |> assert_equal(expected_output);
    },
    "findIndex returns Some of first element in a list that returns true for the find_function" >:: () => {
      let input_list = [10,11,12,13,14,15,16,17,18,19,20,21];
      let find_func = (_, _, i) => i > 15;
      let expected_output = 6;
      input_list |> Odash.findIndex(find_func) |> assert_equal(expected_output);
    },
    "findIndex returns -1 for a list in which no element returns true for the find_function" >:: () => {
      let input_list = [10,11,12,13,14,15,16,17,18,19,20,21];
      let find_func = (_, _, i) => i > 50;
      let expected_output = -1;
      input_list |> Odash.findIndex(find_func) |> assert_equal(expected_output);
    },
    "findIndex returns 0 if all elements return true for the find_function" >:: () => {
      let input_list = [10,11,12,13,14,15,16,17,18,19,20,21];
      let find_func = (_, _, _) => true;
      let expected_output = 0;
      input_list |> Odash.findIndex(find_func) |> assert_equal(expected_output);
    },
    "findIndex returns -1 for an empty list" >:: () => {
      let input_list = [];
      let find_func = (_, _, _) => true;
      let expected_output = -1;
      input_list |> Odash.findIndex(find_func) |> assert_equal(expected_output);
    },
    "findLastIndex returns index of last element in a list that returns true for the find_function" >:: () => {
      let input_list = [10,11,12,13,14,15,16,17,18,19,20,21];
      let find_func = (_, _, i) => i < 15;
      let expected_output = 4;
      input_list |> Odash.findLastIndex(find_func) |> assert_equal(expected_output);
    },
    "some returns true if all elements in the list meets the some_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let some_func = (_, _, i) => i > -1;
      let expected_output = true;
      input_list |> Odash.some(some_func) |> assert_equal(expected_output);
    },
    "some returns true if any element in the list meets the some_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let some_func = (_, _, i) => i == 5;
      let expected_output = true;
      input_list |> Odash.some(some_func) |> assert_equal(expected_output);
    },
    "some returns false for a list in which no element returns true for the some_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let some_func = (_, _, i) => i > 50;
      let expected_output = false;
      input_list |> Odash.some(some_func) |> assert_equal(expected_output);
    },
    "some returns false for an empty list" >:: () => {
      let input_list = [];
      let some_func = (_, _, _) => true;
      let expected_output = false;
      input_list |> Odash.some(some_func) |> assert_equal(expected_output);
    },
    "every returns true if all elements of list meet every_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let every_func = (_, _, i) => i > -1;
      let expected_output = true;
      input_list |> Odash.every(every_func) |> assert_equal(expected_output);
    },
    "every returns false if only some elements of list meet every_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let every_func = (_, _, i) => i > 5;
      let expected_output = false;
      input_list |> Odash.every(every_func) |> assert_equal(expected_output);
    },
    "every returns false if no elements of list meet every_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let every_func = (_, _, i) => i > 50;
      let expected_output = false;
      input_list |> Odash.every(every_func) |> assert_equal(expected_output);
    },
    "includesBy returns true if includes_func is met in list once" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,11];
      let element = 10;
      let includes_func = (arr_item, compare_item) => arr_item > compare_item;
      let expected_output = true;
      input_list |> Odash.includesBy(includes_func, element) |> assert_equal(expected_output);
    },
    "includesBy returns true if includes_func is always true" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,11];
      let element = 10;
      let includes_func = (_, _) => true;
      let expected_output = true;
      input_list |> Odash.includesBy(includes_func, element) |> assert_equal(expected_output);
    },
    "includesBy returns false if includes_func is never met in list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9];
      let element = 10;
      let includes_func = (arr_item, compare_item) => arr_item > compare_item;
      let expected_output = false;
      input_list |> Odash.includesBy(includes_func, element) |> assert_equal(expected_output);
    },
    "includesBy returns false for empty list" >:: () => {
      let input_list = [];
      let element = 3;
      let includes_func = (_, _) => true;
      let expected_output = false;
      input_list |> Odash.includesBy(includes_func, element) |> assert_equal(expected_output);
    },
    "includes returns true if element is in list once" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let element = 3;
      let expected_output = true;
      input_list |> Odash.includes(element) |> assert_equal(expected_output);
    },
    "includes returns true if list is composed of a bunch of specified element" >:: () => {
      let input_list = [3,3,3,3,3,3,3];
      let element = 3;
      let expected_output = true;
      input_list |> Odash.includes(element) |> assert_equal(expected_output);
    },
    "includes returns false if specified element is not in list" >:: () => {
      let input_list = [0,1,2,4,5,6,7,8,9,10,11];
      let element = 3;
      let expected_output = false;
      input_list |> Odash.includes(element) |> assert_equal(expected_output);
    },
    "includes returns false for empty list" >:: () => {
      let input_list = [];
      let element = 3;
      let expected_output = false;
      input_list |> Odash.includes(element) |> assert_equal(expected_output);
    },
    "partition splits a list in items true and false for predicate, preserving order" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let partition_func = i => (i mod 2) == 0;
      let expected_output = ([0,2,4,6,8,10], [1,3,5,7,9,11]);
      input_list |> Odash.partition(partition_func) |> assert_equal(expected_output);
    },
    "partition puts all items in first list if partition function true for all" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let partition_func = _ => true;
      let expected_output = ([0,1,2,3,4,5,6,7,8,9,10,11], []);
      input_list |> Odash.partition(partition_func) |> assert_equal(expected_output);
    },
    "partition puts all items in second list if partition function false for all" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let partition_func = _ => false;
      let expected_output = ([], [0,1,2,3,4,5,6,7,8,9,10,11]);
      input_list |> Odash.partition(partition_func) |> assert_equal(expected_output);
    },
    "partition returns two empty lists when given and empty list" >:: () => {
      let input_list = [];
      let partition_func = i => i mod 2 == 0;
      let expected_output = ([], []);
      input_list |> Odash.partition(partition_func) |> assert_equal(expected_output);
    },
    "sampleSize returns a random sample of the specified size from the list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let sample_size = 4;
      let sample = input_list |> Odash.sampleSize(sample_size)
      let _length_assertion = sample |> List.length |> assert_equal(sample_size);
      let all_include = (i) => Odash.includes(i, input_list) |> assert_equal(true)
      let _inclusion_assertion = sample |> List.map(all_include);
    },
    "sampleSize returns a shuffled list if the specified size is greater than the list size" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let sample_size = 100;
      let sample = input_list |> Odash.sampleSize(sample_size)
      let _length_assertion = sample |> List.length |> assert_equal(List.length(input_list));
      let all_include = (i) => Odash.includes(i, input_list) |> assert_equal(true)
      let _inclusion_assertion = sample |> List.map(all_include);
    },
    "sampleSize returns an empty list if given an empty list" >:: () => {
      let input_list = [];
      let sample_size = 1;
      input_list |> Odash.sampleSize(sample_size) |> assert_equal(input_list);
    },
    "sampleSize returns an empty list sample size specified as zero" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let sample_size = 0;
      let expected_output = [];
      input_list |> Odash.sampleSize(sample_size) |> assert_equal(expected_output);
    },
    "sample returns a random item from a list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let sample = Odash.sample(input_list)
      input_list |> Odash.includes(sample) |> assert_equal(true);
    },
    "shuffle returns a random shuffle of the input_list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let shuffled_list = input_list |> Odash.shuffle;
      let _length_assertion = shuffled_list |> List.length |> assert_equal(List.length(input_list));
      let all_include = (i) => Odash.includes(i, input_list) |> assert_equal(true)
      let _inclusion_assertion = shuffled_list |> List.map(all_include);
    },
    "size returns list length" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let expected_output = 12;
      input_list |> Odash.size |> assert_equal(expected_output);
    },
    "simpleSortBy sorts items in ascending order after they are passed through sort_func" >:: () => {
      let input_list = [0,1,9,7,9,5];
      let expected_output = [0,1,5,7,9,9]
      input_list |> Odash.simpleSortBy(Odash.identity) |> assert_equal(expected_output);
    },
    "sortBy sorts items by multiple sort_funcs" >:: () => {
      let sammy: dog = { name: "Sammy", weight: 14 };
      let reilly: dog = { name: "Reilly", weight: 12 };
      let abby: dog = { name: "Abby", weight: 12 };

      let input_list = [reilly, sammy, abby];
      let first_sort: (dog => int) = (d) => d.weight;
      let second_sort: (dog => int) = (d) => String.length(d.name);
      let expected_output = [abby, reilly, sammy];
      input_list |> Odash.sortBy([first_sort, second_sort]) |> assert_equal(expected_output);
    },
    "sortBy returns list as-is when not passed any sort_funcs" >:: () => {
      let sammy: dog = { name: "Sammy", weight: 14 };
      let reilly: dog = { name: "Reilly", weight: 12 };
      let abby: dog = { name: "Abby", weight: 12 };

      let input_list = [reilly, sammy, abby];
      input_list |> Odash.sortBy([]) |> assert_equal(input_list);
    },
    "orderBy sorts items by multiple sort_funcs in ascending order when no sort_orders are passed" >:: () => {
      let sammy: dog = { name: "Sammy", weight: 14 };
      let reilly: dog = { name: "Reilly", weight: 12 };
      let abby: dog = { name: "Abby", weight: 12 };

      let input_list = [reilly, sammy, abby];
      let first_sort: (dog => int) = (d) => d.weight;
      let second_sort: (dog => int) = (d) => String.length(d.name);
      let function_list = [first_sort, second_sort];
      let order_list = [];
      let expected_output = [abby, reilly, sammy];
      input_list |> Odash.orderBy(function_list, order_list) |> assert_equal(expected_output);
    },
    "orderBy returns list as-is when not passed any sort_funcs" >:: () => {
      let sammy: dog = { name: "Sammy", weight: 14 };
      let reilly: dog = { name: "Reilly", weight: 12 };
      let abby: dog = { name: "Abby", weight: 12 };

      let input_list = [reilly, sammy, abby];
      let function_list = [];
      let order_list = [Odash.Desc, Odash.Asc, Odash.Desc];
      input_list |> Odash.orderBy(function_list, order_list) |> assert_equal(input_list);
    },
    "orderBy sorts items by multiple sort_funcs and sort_orders when passed" >:: () => {
      let sammy: dog = { name: "Sammy", weight: 14 };
      let reilly: dog = { name: "Reilly", weight: 12 };
      let abby: dog = { name: "Abby", weight: 12 };

      let input_list = [reilly, sammy, abby];
      let first_sort: (dog => int) = (d) => d.weight;
      let second_sort: (dog => int) = (d) => String.length(d.name);
      let function_list = [first_sort, second_sort];
      let order_list = [Odash.Desc, Odash.Asc];
      let expected_output = [sammy, abby, reilly];
      input_list |> Odash.orderBy(function_list, order_list) |> assert_equal(expected_output);
    },
    /* TODO: figure out why this doesn't type check
    "sample raises invalid if passed an empty list" >:: () => {
      let input_list = [];
      let expected_exception = "input_list cannot be empty!";
      Odash.sample(input_list) |> assert_raises(expected_exception);
    }, */
    "join joins all elements of a list of strings using supplied join string" >:: () => {
      let input_list = ["Reilly", "Sammy", "Abby", "Gus"];
      let join_string = "<and>";
      let expected_output = "Reilly<and>Sammy<and>Abby<and>Gus";
      input_list |> Odash.join(join_string) |> assert_equal(expected_output);
    },
    "join returns an empty string for an empty list" >:: () => {
      let input_list = [];
      let join_string = "<and>";
      let expected_output = "";
      input_list |> Odash.join(join_string) |> assert_equal(expected_output);
    },
    "join can return list joined by an empty string" >:: () => {
      let input_list = ["Reilly", "Sammy", "Abby", "Gus"];
      let join_string = "";
      let expected_output = "ReillySammyAbbyGus";
      input_list |> Odash.join(join_string) |> assert_equal(expected_output);
    },
    "uniqWith returns list filtered by uniqueness, as defined the comparison func" >:: () => {
      let starting_list = [0.0, 1.0, 1.1, 2.2, 3.3, 3.0, 4.4];
      let uniq_with_func = (earlier_item, later_item) => earlier_item == floor(later_item);
      let expected_output = [0.0, 1.0, 2.2, 3.3, 3.0, 4.4];
      starting_list |> Odash.uniqWith(uniq_with_func) |> assert_equal(expected_output);
    },
    "uniqWith returns first list as-is if comparison func always returns false" >:: () => {
      let starting_list = [0.0, 1.0, 1.1, 2.2, 3.3, 3.0, 4.4];
      let uniq_with_func = (_, _) => false;
      starting_list |> Odash.uniqWith(uniq_with_func) |> assert_equal(starting_list);
    },
    "uniqWith returns list with only first item if comparison func always returns true" >:: () => {
      let starting_list = [0.0, 1.0, 1.1, 2.2, 3.3, 3.0, 4.4];
      let uniq_with_func = (_, _) => true;
      let expected_output = [0.0];
      starting_list |> Odash.uniqWith(uniq_with_func) |> assert_equal(expected_output);
    },
    "uniqWith returns empty list if passed an empty list" >:: () => {
      let starting_list = [];
      let uniq_with_func = (_, _) => true;
      starting_list |> Odash.uniqWith(uniq_with_func) |> assert_equal(starting_list);
    },
    "uniqBy returns list filtered by uniqueness, after each item is passed through transform_func" >:: () => {
      let starting_list = [0.0, 1.0, 1.1, 2.2, 3.3, 3.0, 4.4];
      let uniq_by_func = i => floor(i);
      let expected_output = [0.0, 1.0, 2.2, 3.3, 4.4];
      starting_list |> Odash.uniqBy(uniq_by_func) |> assert_equal(expected_output);
    },
    "uniqBy returns empty list if passed an empty list" >:: () => {
      let starting_list = [];
      let uniq_by_func = _ => true;
      starting_list |> Odash.uniqBy(uniq_by_func) |> assert_equal(starting_list);
    },
    "uniq returns list filtered by uniqueness" >:: () => {
      let starting_list = [0.0, 1.0, 1.1, 2.2, 3.3, 1.1, 3.0, 3.0, 4.4, 0.0];
      let expected_output = [0.0, 1.0, 1.1, 2.2, 3.3, 3.0, 4.4];
      starting_list |> Odash.uniq |> assert_equal(expected_output);
    },
    "uniq returns empty list if passed an empty list" >:: () => {
      let starting_list = [];
      starting_list |> Odash.uniq |> assert_equal(starting_list);
    },
    "intersectionWith returns list filtered by intersection with other lists, as defined the comparison func" >:: () => {
      let first_list = [0.0, 1.0, 1.1, 2.2, 3.3, 4.0, 0.0];
      let second_list = [0.0, 1.1, 2.2, 2.2, 1.1, 4.3];
      let third_list = [0.0, 1.9, 2.2, 2.2, 3.0, 4.4];
      let list_of_lists = [first_list, second_list, third_list];
      let intersection_with_func = (earlier_item, later_item) => earlier_item == floor(later_item);
      let expected_output = [0.0, 1.0, 4.0, 0.0];
      list_of_lists |> Odash.intersectionWith(intersection_with_func) |> assert_equal(expected_output);
    },
    "intersectionWith returns first list as-is if comparison func always returns true" >:: () => {
      let first_list = [0.0, 1.0, 1.1, 2.2, 3.3, 4.0, 0.0];
      let second_list = [0.0, 1.1, 2.2, 2.2, 1.1, 4.3];
      let third_list = [0.0, 1.9, 2.2, 2.2, 3.0, 4.4];
      let list_of_lists = [first_list, second_list, third_list];
      let intersection_with_func = (_, _) => true;
      list_of_lists |> Odash.intersectionWith(intersection_with_func) |> assert_equal(first_list);
    },
    "intersectionWith returns an empty list if comparison func always returns false" >:: () => {
      let first_list = [0.0, 1.0, 1.1, 2.2, 3.3, 4.0, 0.0];
      let second_list = [0.0, 1.1, 2.2, 2.2, 1.1, 4.3];
      let third_list = [0.0, 1.9, 2.2, 2.2, 3.0, 4.4];
      let list_of_lists = [first_list, second_list, third_list];
      let intersection_with_func = (_, _) => false;
      let expected_output = [];
      list_of_lists |> Odash.intersectionWith(intersection_with_func) |> assert_equal(expected_output);
    },
    "intersectionWith returns empty list if passed an empty list" >:: () => {
      let list_of_lists = [];
      let intersection_with_func = (_, _) => true;
      list_of_lists |> Odash.intersectionWith(intersection_with_func) |> assert_equal(list_of_lists);
    },
    "intersectionBy returns first list filtered by elements present in other lists, after each item is passed through transform_func" >:: () => {
      let first_list = [0.0, 1.0, 1.1, 2.2, 3.3, 4.0, 0.0];
      let second_list = [0.0, 1.1, 2.2, 2.2, 1.1, 4.3];
      let third_list = [0.0, 1.9, 2.2, 2.2, 3.0, 4.4];
      let list_of_lists = [first_list, second_list, third_list];
      let intersection_by_func = item => floor(item);
      let expected_output = [0.0, 1.0, 1.1, 2.2, 4.0, 0.0];
      list_of_lists |> Odash.intersectionBy(intersection_by_func) |> assert_equal(expected_output);
    },
    "intersectionBy returns empty list if passed an empty list" >:: () => {
      let list_of_lists = [];
      let intersection_by_func = _ => true;
      list_of_lists |> Odash.intersectionBy(intersection_by_func) |> assert_equal(list_of_lists);
    },
    "intersection returns first list filtered by elements present in other lists" >:: () => {
      let first_list = [0.0, 1.0, 1.1, 2.2, 3.3, 4.0, 0.0];
      let second_list = [0.0, 1.1, 2.2, 2.2, 1.1, 4.3];
      let third_list = [0.0, 1.9, 2.2, 2.2, 3.0, 4.4];
      let list_of_lists = [first_list, second_list, third_list];
      let expected_output = [0.0, 2.2, 0.0];
      list_of_lists |> Odash.intersection |> assert_equal(expected_output);
    },
    "intersection returns empty list if passed an empty list" >:: () => {
      let list_of_lists = [];
      list_of_lists |> Odash.intersection |> assert_equal(list_of_lists);
    },
    "xorWith returns unique items only on one of provided lists, ordered by position within and between lists, as defined the comparison func" >:: () => {
      let first_list = [0.1, 0.0, 1.9, 3.5];
      let second_list = [0.3, 1.0, 2.2];
      let third_list = [4.0, 2.0];
      let list_of_lists = [first_list, second_list, third_list];
      let xor_with_func = (list_item, other_list_item) => floor(list_item) == other_list_item;
      let expected_output = [0.1, 3.5, 0.3, 1.0, 4.0, 2.0];
      list_of_lists |> Odash.xorWith(xor_with_func) |> assert_equal(expected_output);
    },
    "xorWith returns flattened lists if comparison func always returns false" >:: () => {
      let first_list = [0.0, 1.0, 1.1, 2.2, 3.3, 4.0, 0.0];
      let second_list = [0.0, 1.1, 2.2, 2.2, 1.1, 4.3];
      let third_list = [0.0, 1.9, 2.2, 2.2, 3.0, 4.4];
      let list_of_lists = [first_list, second_list, third_list];
      let xor_with_func = (_, _) => false;
      let expected_output = Odash.concat(list_of_lists);
      list_of_lists |> Odash.xorWith(xor_with_func) |> assert_equal(expected_output);
    },
    "xorWith returns an empty list if comparison func always returns true" >:: () => {
      let first_list = [0.0, 1.0, 1.1, 2.2, 3.3, 4.0, 0.0];
      let second_list = [0.0, 1.1, 2.2, 2.2, 1.1, 4.3];
      let third_list = [0.0, 1.9, 2.2, 2.2, 3.0, 4.4];
      let list_of_lists = [first_list, second_list, third_list];
      let xor_with_func = (_, _) => true;
      let expected_output = [];
      list_of_lists |> Odash.xorWith(xor_with_func) |> assert_equal(expected_output);
    },
    "xorWith returns empty list if passed an empty list" >:: () => {
      let list_of_lists = [];
      let xor_with_func = (_, _) => true;
      list_of_lists |> Odash.xorWith(xor_with_func) |> assert_equal(list_of_lists);
    },
    "xorBy returns items present in only one of passed lists, after each item is passed through transform_func" >:: () => {
      let first_list = [0.1, 0.0, 1.9, 3.5];
      let second_list = [0.3, 1.0, 2.2];
      let third_list = [4.0, 2.0];
      let list_of_lists = [first_list, second_list, third_list];
      let xor_by_func = item => floor(item);
      let expected_output = [3.5, 4.0];
      list_of_lists |> Odash.xorBy(xor_by_func) |> assert_equal(expected_output);
    },
    "xorBy returns empty list if passed an empty list" >:: () => {
      let list_of_lists = [];
      let xor_by_func = _ => true;
      list_of_lists |> Odash.xorBy(xor_by_func) |> assert_equal(list_of_lists);
    },
    "xor returns items present in only one of passed lists" >:: () => {
      let first_list = [0, 0, 5, 1, 3, 6];
      let second_list = [1, 6, 2];
      let third_list = [6, 4, 2, 5, 6];
      let list_of_lists = [first_list, second_list, third_list];
      let expected_output = [0, 3, 4];
      list_of_lists |> Odash.xor |> assert_equal(expected_output);
    },
    "xor returns empty list if passed an empty list" >:: () => {
      let list_of_lists = [];
      list_of_lists |> Odash.xor |> assert_equal(list_of_lists);
    },
    "unionWith returns unique items only on one of provided lists, ordered by position within and between lists, as defined the comparison func" >:: () => {
      let first_list = [0.1, 0.0, 1.9, 0.8, 3.5];
      let second_list = [0.3, 1.0, 2.2];
      let third_list = [4.0, 2.0, 1.4];
      let list_of_lists = [first_list, second_list, third_list];
      let union_with_func = (list_item, other_list_item) => list_item == floor(other_list_item);
      let expected_output = [0.1, 0.0, 1.9, 3.5, 1.0, 2.2, 4.0, 2.0];
      list_of_lists |> Odash.unionWith(union_with_func) |> assert_equal(expected_output);
    },
    "unionWith returns flattened lists if comparison func always returns false" >:: () => {
      let first_list = [0.0, 1.0, 1.1, 2.2, 3.3, 4.0, 0.0];
      let second_list = [0.0, 1.1, 2.2, 2.2, 1.1, 4.3];
      let third_list = [0.0, 1.9, 2.2, 2.2, 3.0, 4.4];
      let list_of_lists = [first_list, second_list, third_list];
      let union_with_func = (_, _) => false;
      let expected_output = Odash.concat(list_of_lists);
      list_of_lists |> Odash.unionWith(union_with_func) |> assert_equal(expected_output);
    },
    "unionWith returns a list with only the first element of the first list if comparison func always returns true" >:: () => {
      let first_list = [0.0, 1.0, 1.1, 2.2, 3.3, 4.0, 0.0];
      let second_list = [0.0, 1.1, 2.2, 2.2, 1.1, 4.3];
      let third_list = [0.0, 1.9, 2.2, 2.2, 3.0, 4.4];
      let list_of_lists = [first_list, second_list, third_list];
      let union_with_func = (_, _) => true;
      let expected_output = [0.0];
      list_of_lists |> Odash.unionWith(union_with_func) |> assert_equal(expected_output);
    },
    "unionWith returns empty list if passed an empty list" >:: () => {
      let list_of_lists = [];
      let union_with_func = (_, _) => true;
      list_of_lists |> Odash.unionWith(union_with_func) |> assert_equal(list_of_lists);
    },
    "unionBy returns unique list of all items present in any passed list, after each item is passed through transform_func" >:: () => {
      let first_list = [0.1, 0.0, 1.9, 3.5];
      let second_list = [0.3, 1.0, 2.2];
      let third_list = [4.0, 2.0];
      let list_of_lists = [first_list, second_list, third_list];
      let union_by_func = item => floor(item);
      let expected_output = [0.1, 1.9, 3.5, 2.2, 4.0];
      list_of_lists |> Odash.unionBy(union_by_func) |> assert_equal(expected_output);
    },
    "unionBy returns empty list if passed an empty list" >:: () => {
      let list_of_lists = [];
      let union_by_func = _ => false;
      list_of_lists |> Odash.unionBy(union_by_func) |> assert_equal(list_of_lists);
    },
    "union returns unique list of all items present in any passed list" >:: () => {
      let first_list = [0, 0, 5, 1, 3, 6];
      let second_list = [1, 6, 2];
      let third_list = [6, 4, 2, 5, 6];
      let list_of_lists = [first_list, second_list, third_list];
      let expected_output = [0, 5, 1, 3, 6, 2, 4];
      list_of_lists |> Odash.union |> assert_equal(expected_output);
    },
    "union returns empty list if passed an empty list" >:: () => {
      let list_of_lists = [];
      list_of_lists |> Odash.union |> assert_equal(list_of_lists);
    },
    "without returns input list excluding all items on the exclusion list" >:: () => {
      let input_list = [0,1,9,7,4,9,5];
      let exclusion_list = [7,9,7,2,8,1];
      let expected_output = [0,4,5];
      input_list |> Odash.without(exclusion_list) |> assert_equal(expected_output);
    },
    "without returns input list as-is if exclusion list is empty" >:: () => {
      let input_list = [0,1,9,7,4,9,5];
      let exclusion_list = [];
      input_list |> Odash.without(exclusion_list) |> assert_equal(input_list);
    },
    "without returns an empty list if passed an empty input list" >:: () => {
      let input_list = [];
      let exclusion_list = [7,9,7,2,8,1];
      input_list |> Odash.without(exclusion_list) |> assert_equal(input_list);
    },
    "zip zips a list of two lists into a list of two-element lists" >:: () => {
      let initial_lists = [[0,2,4,6,8,10],[1,3,5,7,9,11]];
      let expected_output = [[0,1],[2,3],[4,5],[6,7],[8,9],[10,11]]
      initial_lists |> Odash.zip |> assert_equal(expected_output);
    },
    "zip zips many two-element lists into a list of two lists" >:: () => {
      let initial_lists = [[0,1],[2,3],[4,5],[6,7],[8,9],[10,11]]
      let expected_output = [[0,2,4,6,8,10],[1,3,5,7,9,11]];
      initial_lists |> Odash.zip |> assert_equal(expected_output);
    },
    "zip returns an empty list when passed an empty list" >:: () => {
      let empty_list = [];
      empty_list |> Odash.zip |> assert_equal(empty_list);
    },
    "unzip zips treats list of two lists exactly like zip" >:: () => {
      let initial_lists = [[0,2,4,6,8,10],[1,3,5,7,9,11]];
      initial_lists |> Odash.unzip |> assert_equal(Odash.zip(initial_lists));
    },
    "unzip treats many two-element lists exactly like zip" >:: () => {
      let initial_lists = [[0,1],[2,3],[4,5],[6,7],[8,9],[10,11]]
      initial_lists |> Odash.unzip |> assert_equal(Odash.zip(initial_lists));
    },
    "unzip treats an empty list exactly like zip" >:: () => {
      let empty_list = [];
      empty_list |> Odash.unzip |> assert_equal(Odash.zip(empty_list));
    },
];

run_test_tt_main(suite);
