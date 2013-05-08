open Grammar_types
open UDBM_utilities
open Clock_constraint_utilities
open Graph_functions2
open Fernandez_modules
open Zone_stubs

open Graph

module ZVGLT2 =
struct
  type node_ref_t = zone_using_dbm
  type action_t = int
  type lts_t = {nodes:((zone_using_dbm * ((transition *
                                               (zone_using_dbm list)) list)) list) array;
                action_count: int;
                clock_names: string array
               }
  let node_equality =
    function l -> function zone1 -> function zone2 ->
      zone1 = zone2
  let node_name =
    function l -> function zone -> ((string_of_int
                                       zone.zone_location2) ^ " " ^
                                       (dbm_to_string l.clock_names zone.zone_constraint2) )
  let expand_action = function l -> function a -> string_of_int a
  let nodes =
    function l ->
      List.map
        (function (zone, _) -> zone)
        (List.concat
           (Array.to_list
              l.nodes
           )
        )
  let actions =
    function l ->
      Array.to_list
        (Array.init
           (1 + l.action_count) (*The extra action is -1, for time transitions.*)
           (function a-> a - 1)
        )
  let in_adjacency = function l -> function zone ->
    (function a ->
      let
          all_zones_with_departures =
        List.concat
          (Array.to_list l.nodes)
      in
      let
          useful_zones_with_departures =
        List.filter
          (function (zone1, dl) ->
            List.exists
              (function (departure, zone_list) ->
                departure.action = a
                &&
                  (List.exists
                     (node_equality l zone)
                     zone_list
                  )
              )
              dl
          )
          all_zones_with_departures
      in
      let
          useful_zones =
        List.map
          (function (zone1, _) -> zone1)
          useful_zones_with_departures
      in
      useful_zones
    )
end

module ZVGLTS2 = LTS (ZVGLT2)

module ZVGQuotient2 = ZVGLTS2.Quotient_LTS

module ZVGQuotientLTS2 = LTS (ZVGQuotient2)

let lts_of_zone_valuation_graph ta =
  {ZVGLT2.action_count = ta.numactions;
   ZVGLT2.nodes = generate_zone_valuation_graph ta;
   ZVGLT2.clock_names = ta.clock_names
  }

type half_key = ZVGQuotient2.node_ref_t

module type DP_TABLE_TYPE =
sig
  type table (*The 'a type is the co-ordinate type for the table.*)
  val empty_table: unit -> table
  val lookup:
    table ->
    ZVGQuotient2.lts_t ->
    ZVGQuotient2.lts_t ->
    (half_key * half_key) ->
    bool (*Whether it was found or not.*)
  val remove:
    table ->
    ZVGQuotient2.lts_t ->
    ZVGQuotient2.lts_t ->
    (half_key * half_key) ->
    bool (*Whether we needed to remove it or not.*)
  val insert:
    table ->
    ZVGQuotient2.lts_t ->
    ZVGQuotient2.lts_t ->
    (half_key * half_key) ->
    bool (*Whether we needed to remove something before inserting it.*)
end
  
module type TA_RELATION_TYPE =
sig
  val nodes_to_other_nodes:
    ZVGQuotient2.lts_t ->
    ZVGQuotient2.lts_t ->
    half_key ->
    half_key ->
    (((half_key * ZVGQuotient2.action_t * (half_key list)) list) *
        (((half_key list) * ZVGQuotient2.action_t * half_key) list))
end

let out_adjacency l z a
    =
  List.filter
    (function zz ->
      List.exists
        (ZVGQuotient2.node_equality l z)
        (ZVGQuotient2.in_adjacency l zz a)
    )
    (ZVGQuotient2.nodes l)

let out_adjacency_with_delay_earlier l z a
    =
  let rec f ol =
    let ext =
      List.filter
        (function z ->
          not
            (List.exists
               (ZVGQuotient2.node_equality l z)
               ol
            )
        )
        (List.concat
           (List.map
              (function z -> out_adjacency l z (-1))
              ol
           )
        )
    in
    match ext with
    | [] -> ol
    | _ -> f (ext@ol)
  in
  List.concat
    (List.map
       (function z -> out_adjacency l z a)
       (f [z])
    )

let out_adjacency_with_delay_earlier_and_later l z a =
  let rec f ol =
    let ext =
      List.filter
        (function z ->
          not
            (List.exists
               (ZVGQuotient2.node_equality l z)
               ol
            )
        )
        (List.concat
           (List.map
              (function z -> out_adjacency l z (-1))
              ol
           )
        )
    in
    match ext with
    | [] -> ol
    | _ -> f (ext@ol)
  in
  f (out_adjacency_with_delay_earlier l z a)

