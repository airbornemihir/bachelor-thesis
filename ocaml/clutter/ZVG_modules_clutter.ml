open Grammar_types
open UDBM_utilities
open Clock_constraint_utilities
open Graph_functions2
open Fernandez_modules
open Zone_stubs

module ZVGLT =
struct
  type node_ref_t = zone_using_list
  type action_t = int
  type lts_t = {nodes:((zone_using_list * ((transition *
                                              (zone_using_list list)) list)) list) array;
                action_count: int}
  let node_equality =
    function l -> function node_ref1 -> function node_ref2 ->
      (node_ref1 = node_ref2)
  let node_name =
    function l -> function zone -> ((string_of_int
                                       zone.zone_location1) ^ " " ^
                                       (string_of_clock_constraint zone.zone_constraint1) )
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
                  (List.mem zone zone_list)
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

module ZVGLTS = LTS (ZVGLT)
