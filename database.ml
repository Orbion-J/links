open Num
open List

open Utility

type database = Value.database
exception Runtime_error = Errors.Runtime_error

class virtual db_args from_str = object
  val strval : string = from_str
  method virtual from_string : string -> unit
end

let value_of_db_string (value:string) t =
  match TypeUtils.concrete_type t with
    | `Primitive `Bool -> Value.box_bool (value = "true")
    | `Primitive `Char -> Value.box_char (String.get value 0)
    | `Alias (("String", _), _) -> Value.string_as_charlist value
        (* BUG: we should be more principled about detecting strings *)
    | `Application (l, [`Type (`Primitive `Char)]) 
        when Eq.eq Types.Abstype.eq_t l Types.list ->
        Value.string_as_charlist value
    | `Primitive `Int  -> Value.box_int (num_of_string value)
    | `Primitive `Float -> (if value = "" then Value.box_float 0.00      (* HACK HACK *)
                            else Value.box_float (float_of_string value))
    | t -> failwith ("value_of_db_string: unsupported datatype: '" ^ Show.show Types.show_datatype t (*string_of_datatype t*) ^"'")

let execute_command  (query:string) (db: database) : Value.t =
  let result = (db#exec query) in
    begin
      match result#status with
        | `QueryOk -> `Record []
        | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg))
    end

let execute_insert (table_name, field_names, vss) db =
  execute_command (db#make_insert_query (table_name, field_names, vss)) db

let execute_insert_returning (table_name, field_names, vss, returning) db =
  let qs = db#make_insert_returning_query (table_name, field_names, vss, returning) in
  let rec run =
    function
      | [] -> assert false
      | [q] ->
          let result = db#exec q in
            begin
              match result#status with
               | `QueryOk ->
                   let rows = result#get_all_lst in
                     begin
                       match rows with
                         | [[id]] -> Value.box_int (Num.num_of_string id)
                         | _ -> raise (Runtime_error ("Returned the wrong number of results executing " ^ q))
                     end
               | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ q ^ ": " ^ msg))
            end
      | q :: qs ->
          execute_command q db;
          run qs
  in
    run qs

let execute_select (field_types:(string * Types.datatype) list) (query:string) (db : database)
    : Value.t =
  let result = (db#exec query) in
    (match result#status with
       | `QueryOk -> 
           begin
             match field_types with
               | [] ->
                   (* we need to ignore any dummy fields introduced to
                      work around SQL's inability to handle empty
                      records *)
                   `List (map (fun _ -> `Record []) result#get_all_lst)
               | _ ->
                   let row_fields =
	             (let temp_fields = ref [] in
                        (* TBD: factor this out as 
                           result_sig : dbresult -> (string * dbtype * datatype) list *)
                        for count = result#nfields - 1 downto 0 do 
                          try 
                            temp_fields := 
                              (result#fname count,
                               (List.assoc (result#fname count) field_types))
                            :: !temp_fields
                          with NotFound _ -> (* Could probably remove this. *)
                            failwith("Column " ^ (result#fname count) ^ 
                                       " had no type info in query's type spec: " ^
                                       mapstrcat ", " (fun (fld, typ) -> fld ^ ":" ^ 
                                                         Types.string_of_datatype typ)
                                       field_types)
                        done;
                        !temp_fields) in
                   let is_null = (fun (name, _) ->
                                    if name = "null" then true
                                    else if mem_assoc name field_types then false
                                    else assert false) in
                   let null_query = exists is_null row_fields in
                     if null_query then
                       `List (map (fun _ -> `Record []) result#get_all_lst)
                     else
                       `List (map (fun rowvalue ->
                                     `Record (map2 (fun (name, t) fldvalue -> 
			                              name, value_of_db_string fldvalue t)
                                                row_fields rowvalue))
                                result#get_all_lst)
           end
       | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))

let execute_untyped_select (query:string) (db: database) : Value.t =
  let result = (db#exec query) in
    (match result#status with
       | `QueryOk -> 
           `List (map (fun row -> `List (map Value.box_string row)) result#get_all_lst)
       | `QueryError msg -> raise (Runtime_error ("An error occurred executing the query " ^ query ^ ": " ^ msg)))
