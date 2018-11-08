# SandDB: A simple immutable database for the masses

SandDB is:
- Simple: It only does one thing, which is persisting data in a file.
- Easy to use: SandDB's API is extremely small, so you only need to know few functions to use it.
- Type safe: Every common dangerous operation (like parsing) is covered by the Result type, so you will know where to expect errors.
- Immutable:  Database is based on the immutable stack idea, where you can only push onto the stack.
- Crud capable: Even though the database is immutable you still can update and delete records, by shadowing them.
- Version keeping: Every update and delete operation will produce a new version of the affected record, without modifying the original, so you will have all versions of your data.
- Concurrent: SandDB is based on lwt, so every database operation is asynchronous.
- Supports multiple serializers: SandDB supports both json and biniou serialization format thanks to the atdgen library.

## Documentation
[API documentation](https://strykerkkd.github.io/SandDB/)

## How to use it?
This how to is based on the example that you can find in the examples directory.

0. Install SandDB
    ```
    opam install sanddb
    ```

1. Define your record's type with atd

    One of the most important component of SandDB is the atd library, so you must be a little bit familiar with it to be able to use the database.

    You can see below we defined a record which contains a date fields and a data field. It's important to notice that your record's root type's name must be t, because otherwise SandDB can't recognize which type to use. You can learn more about atd [here](https://mjambon.github.io/atdgen-doc/).

    ```
    type t = {
        year : int;
        month : int;
        day : int;
        data: string;
    }
    ```

2. Generate atd serializers for your record's type

    a. Generate with dune
    
    This will generate the serializers in the build directory, so it will keep your work directory clean of generated files.
    
    ```
    ;This rule generates your records type file
    (rule
        (targets record_t.ml record_t.mli)
        (deps record.atd)
        (action (run atdgen -t %{deps})))
    
    ;This rule generates the json serializer
    (rule
        (targets record_j.ml record_j.mli)
        (deps record.atd)
        (action (run atdgen -j %{deps})))

    ;This rule generates the biniou serializer
    (rule
        (targets record_b.ml record_b.mli)
        (deps record.atd)
        (action (run atdgen -b %{deps})))
    ```
    ----

    b. Generate maualy by using atdgen

    In this case you will generate the serializers in your working directory.

    ```shell
    $ atdgen -t record.atd     # produces OCaml type definitions
    $ atdgen -j record.atd     # produces OCaml code dealing with JSON
    $ atdgen -b record.atd     # produces OCaml code dealing with Biniou
	```
    
    Notice: You don't need to generate both json and biniou serializer if you are only using one of them.

3. Create the database

    Database creation only needs two things:
    - Database file, which in our case will be `test.txt`
    - Data serializer, which we generated in the previous steps

    ```ocaml
    (*Creating a json based database with test.txt file and Record_j generated serializer*)
    let database = Sanddb.create_json_database "test.txt" (module Record_j)

    (*or*)

    (*Creating a biniou based database with test.txt file and Record_b generated serializer*)
    let database = Sanddb.create_biniou_database "test.txt" (module Record_b)
    ```


4. Insert record

    When you are inserting a record into the database you basically appending the record into the database file with a generated uuid, which will be the record's id.

    ```ocaml
    let record = { year = 2018; month = 4; day = 30; data="Some data 1"}
    Sanddb.insert_record database record
    ```

5. Insert shadowing record

    One of the main feature of SandDb is that it's immutable, which is a good thing, but sometimes you want to update or delete a record. That's the time when you want to use a shadowing insert, because you can insert with it a record, which will overshadow the older record, so the older record won't be visible. This is achived by using the old record's id for the new record.

    ```ocaml
    let shadowing_record = { year = 2018; month = 5; day = 1; data="Some data 2"}
    Sanddb.insert_shadowing_record database id shadowing_record
    ```

6. Read all records

    This will read out every record in the database both visible and shadowed record.
    You will get a list of tuples, where the first item is the oldest and the last item is the newest in the list. The tuple will consist of a record id and the record's content.

    ```ocaml
    Sanddb.read_all_records database ()
    ```

7. Read visible records

    This will only read out the visible records in the database and will give back a list of tuples, where the first item is the newest and the last item is the oldest. So the order of the list items will be different in this case.

    ```ocaml
    Sanddb.read_visible_records database ()
    ```
