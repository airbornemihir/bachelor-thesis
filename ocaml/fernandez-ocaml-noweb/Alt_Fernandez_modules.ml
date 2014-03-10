open Graph

module type LTS_STATE_TYPE =
sig
  include Sig.COMPARABLE
  val state_name: t -> string
end

module type LTS_ACTION_TYPE = 
sig
  include Sig.ORDERED_TYPE_DFT
  type action = t
  module ActionMap : Map.S with type key=action
  val action_names: string ActionMap.t
end
  
module type LTS_TYPE =
sig
  module A: LTS_ACTION_TYPE
  include Sig.P with type E.label = A.t
  type action = E.label
  module ActionMap : Map.S with type key=action
  val action_names: string ActionMap.t
  val graph_attributes : t -> Graphviz.DotAttributes.graph list
  val default_vertex_attributes : t -> Graphviz.DotAttributes.vertex list
  val vertex_name : V.t -> string
  val vertex_attributes : V.t -> Graphviz.DotAttributes.vertex list
  val get_subgraph : V.t -> Graphviz.DotAttributes.subgraph option
  val default_edge_attributes : t -> Graphviz.DotAttributes.edge list
  val edge_attributes : E.t -> Graphviz.DotAttributes.edge list
end

module LTS_Functor = 
  functor (S : LTS_STATE_TYPE) -> 
    functor (A : LTS_ACTION_TYPE) ->
      (struct

        module Base = Persistent.Digraph.ConcreteBidirectionalLabeled
          (S) (A)
        include Base

        module A = A
          
        let vertex_name = S.state_name

        type action = A.t
        module ActionMap = A.ActionMap
        let action_names = A.action_names
          
        let mod_add_edge add_edge g v1 v2 =
          if
            A.ActionMap.mem A.default (A.action_names)
          then
            add_edge g v1 v2
          else
            raise (Invalid_argument "Invalid action.")
              
        let mod_add_edge_e add_edge_e g e =
          if
            A.ActionMap.mem (E.label e) (A.action_names)
          then
            add_edge_e g e
          else
            raise (Invalid_argument "Invalid action.")
              
        let add_edge_e = mod_add_edge_e add_edge_e

        let add_edge = mod_add_edge add_edge

        let graph_attributes g  = []

        let default_vertex_attributes g = []

        let vertex_attributes v = []

        let get_subgraph v = None

        let default_edge_attributes g = []

        let edge_attributes (_, a, _) = [`Label (ActionMap.find a action_names)]
       end
         : LTS_TYPE  with type V.t = S.t
                     and type V.label = S.t
                     and type A.t = A.t
      )

module LTS_Dot_Functor =
  functor (LTS: LTS_TYPE) ->
struct
  include Graphviz.Dot (LTS)
end

module Fernandez_Functor =
  functor (LTS: LTS_TYPE) ->
struct
  module StateSet = Set.Make(LTS.V)
  module InfoMap = Map.Make(
    struct
      type t = LTS.action * LTS.V.t
      let compare (a1, v1) (a2, v2) =
        let
            temp1 = LTS.A.compare a1 a2
        in
        if
          temp1 = 0
        then
          LTS.V.compare v1 v2
        else
          temp1
    end
  )
  type block = {
    node_refs: StateSet.t;
    info: int InfoMap.t
  }
  module BlockSet = Set.Make(
    struct
      type t = block
      let compare = Pervasives.compare
    end
  )
  type partition = BlockSet.t

  type splitter = Simple of block | Compound of block * splitter * splitter

  let rec verify_splitter found expected =
    match
      (found, expected)
    with
    | (Simple b1, Simple b2) ->
      StateSet.equal b1.node_refs b2.node_refs
    | (Compound (b1, s11, s12), Compound (b2, s21, s22)) ->
      (StateSet.equal b1.node_refs b2.node_refs) &&
        (verify_splitter s11 s12) && (verify_splitter s21 s22)

  let rec two_way_update_tree t x x1 x2 =
    match t with
    | Simple t0 ->
      if
        StateSet.equal t0.node_refs x.node_refs
      then
        (Compound (x, Simple x1, Simple x2), true)
      else
        (t, false)
    | Compound (t0, t1, t2) ->
      match
        (two_way_update_tree t1 x x1 x2)
      with
      | (_, false) -> (
        match
          (two_way_update_tree t2 x x1 x2)
        with
        | (_, false) -> (t, false)
        | (tt2, true) -> (Compound (t0, t1, tt2), true)
      )
      | (tt1, true) -> (Compound (t0, tt1, t2), true)

  let rec two_way_update_queue w x x1 x2 =
    match w with
      [] -> [Compound (x, Simple x1, Simple x2)]
    | t::ts -> (match (two_way_update_tree t x x1 x2) with
      (_, false) -> t::(two_way_update_queue ts x x1 x2)
      | (tt, true) -> tt::ts
    )

  let get_info l node_refs =
    LTS.fold_vertex
      (function v ->
        function m ->
          LTS.ActionMap.fold
            (function action ->
              function action_name ->
                function m ->
                  InfoMap.add
                    (action, v)
                    (LTS.fold_succ_e
                       (function e ->
                         function count ->
                           if
                             (*The following expression is meant to
                               compare the two actions using their
                               own semantics, instead of
                               Pervasives.(=), because the latter is
                               inaccurate and because lame data
                               hiding only allows this way of using
                               the actions' own semantics for comparison.*)
                             (LTS.ActionMap.cardinal
                                (LTS.ActionMap.add
                                   action
                                   ()
                                   (LTS.ActionMap.add
                                      (LTS.E.label e)
                                      ()
                                      LTS.ActionMap.empty
                                   )
                                )
                              = 1
                                &&
                                  (StateSet.mem
                                     (LTS.E.dst e)
                                     node_refs
                                  )
                             )
                           then
                             count + 1
                           else
                             count
                       )
                       l
                       v
                       0
                    )
                    m
            )
            LTS.action_names
            m
      )
      l
      InfoMap.empty

  let get_block_from_node_refs l node_refs =
    {node_refs = node_refs;
     info =
        (get_info l node_refs)
    }

  let simple_split_block_on_action l x b action =
    match
      StateSet.partition
        (function xelem ->
          LTS.fold_succ_e
            (function e ->
              Pervasives.(||)
                (
                  (StateSet.mem
                     (LTS.E.dst e)
                     b.node_refs
                  ) &&
                    LTS.ActionMap.cardinal
                    (LTS.ActionMap.add
                       action
                       ()
                       (LTS.ActionMap.add
                          (LTS.E.label e)
                          ()
                          LTS.ActionMap.empty
                       )
                    )
                  = 1
                )
            )
            l
            xelem
            false
        )
        x.node_refs
    with
      (x1indices, x2indices) ->
        (get_block_from_node_refs l x1indices, get_block_from_node_refs l x2indices)

  let compound_split_block_on_action l x b b1 b2 a =
    let 
        (b1, b2) =
      if
        ((StateSet.cardinal b2.node_refs) < (StateSet.cardinal b1.node_refs))
      then
        (b2, b1)
      else
        (b1, b2)
    in
    match
      (StateSet.partition
         (function xelem -> (InfoMap.find (a, xelem) b1.info) > 0)
         x.node_refs)
    with
      (y1, y2) ->
        (match
            (StateSet.partition
               (function xelem ->
                 (InfoMap.find (a, xelem) b1.info) < (InfoMap.find (a, xelem) b.info)
               )
               y1
               ,
             StateSet.partition
               (function xelem ->
                 (InfoMap.find (a, xelem) b.info) > 0
               )
               y2)
         with
           ((x3noderefs, x1noderefs), (x2noderefs, x4noderefs)) ->
             ((get_block_from_node_refs l x3noderefs,
               get_block_from_node_refs l x1noderefs),
              (get_block_from_node_refs l x2noderefs,
               get_block_from_node_refs l x4noderefs))
        )

end

module NK_yes_table_functor =
  functor (LTS: LTS_TYPE) ->
    (struct

      module StatePairMap = Map.Make(
        struct
          type t = LTS.V.t * LTS.V.t
          let compare (p1, q1) (p2, q2) =
            let
                temp1 = LTS.V.compare p1 p2
            in
            if
              temp1 = 0
            then
              LTS.V.compare q1 q2
            else
              temp1
        end
      )
        
      include StatePairMap

      let conditional_add
          ((p, q):LTS.V.t * LTS.V.t)
          ((n, k): int * int)
          (m: ((int * int) list) t) : (((int * int) list) t) =
        let redundant _ _ = false
        in
        try
          (let
              l = find (p, q) m
           in
           if
             (redundant (n, k) l)
           then
             m
           else
             add (p, q) ((n, k)::l) m
          )
        with
        | Not_found -> add (p, q) [(n, k)] m

      let conditional_remove
          ((p, q):LTS.V.t * LTS.V.t)
          ((n, k): int * int)
          (m: ((int * int) list) t) : (((int * int) list) t) =
        try
          (let
              l = find (p, q) m
           in
           add (p, q) (List.filter (Pervasives.(<>) (n, k)) l) m
          )
        with
        | Not_found -> m

     end)

module HM_Formula =
  functor (LTS: LTS_TYPE) ->
    (struct

      type hm_formula =
      | DIAMOND of LTS.A.t * hm_formula
      | BOX of LTS.A.t * hm_formula
      | AND of hm_formula list (*AND [] is true.*)
      | OR of hm_formula list (* OR [] is false.*)

      let rec negation formula =
        match formula with
        | DIAMOND (a, formula) -> BOX (a, negation formula)
        | BOX (a, formula) -> DIAMOND (a, negation formula)
        | AND formula_list -> OR (List.map negation formula_list)
        | OR formula_list -> AND (List.map negation formula_list)
          
     end)

module NK_Rel =
  functor (LTS: LTS_TYPE) ->
    (struct

      include HM_Formula(LTS)

      let check_entry_yes_table yes_table p q n k =
        List.exists
          (function (p1, q1, n1, k1) ->
            (p1 = p) && (q1 = q) && (n1 >= n) && (k1 >= k))
          yes_table

      let check_entry_no_table no_table p q n k = 
        List.exists
          (function (p1, q1, n1, k1) ->
            (p1 = p) && (q1 = q) && (n1 <= n) && (k1 <= k))
          no_table

      let add_entry_yes_table yes_table p q n k =
        (p, q, n, k)::
          (List.filter
             (function (p1, q1, n1, k1) ->
               (p1 <> p) || (q1 <> q) || (n1 > n) || (k1 > k))
             yes_table)

      let add_entry_no_table no_table p q n k =
        (p, q, n, k)::
          (List.filter
             (function (p1, q1, n1, k1) ->
               (p1 <> p) || (q1 <> q) || (n1 < n) || (k1 < k))
             no_table)

      let remove_entry_yes_table yes_table p q n k =
        List.filter
          (function (p1, q1, n1, k1) ->
            (p1 <> p) || (q1 <> q) || (n1 < n) || (k1 < k))
          yes_table

      let create_yes_table () = []

      let create_no_table () = []

      (* we assume that p is the challenger's position in lts1 and q is
         the defender's position in lts2. thus, if the challenger switches to
         q now and makes a move in lts2, then we use up one alternation,
         otherwise we have the same number of alternations remaining.*)
      let rec
	  checknkRel
	  lts1
	  lts2
	  p
	  q
	  n
	  k
          yes_table
          no_table
          (* rel is some specific relation, can be a prebisim or a
             simulation equivalence or a bisimulation *)
	  rel = 
        let
            () =
          Printf.printf
            "pushed p = %s, q = %s, n = %s, k = %s\n"
            (LTS.vertex_name p)
            (LTS.vertex_name q)
            (string_of_int n)
            (string_of_int k)
        in
        let
            result = (
          if k = 0 then (true, [], [], yes_table, no_table)
          (* The function checkEntry checks if there is at least one
             entry for p, q with both remaining number of alternations
             and remaining number of rounds to be greater than n and k
             respectively. If not then p, q with current n, k values
             are added in the function getEntry and *)
          else if check_entry_yes_table yes_table p q n k  
          then (true, [], [], yes_table, no_table)
          else if check_entry_no_table no_table p q n k
          then (false, [], [], yes_table, no_table)
          else (
	    (* now we can remove all entries in which the n-value is not
               greater than n and the k-value is not greater than k. *)
	    let yes_table = add_entry_yes_table yes_table p q n k in
	    (* for each successor p' of p, check if that is simulated by
               a successor q' of q *)
	    let
	        (v_p, l_p, yes_table, no_table) =
              (* This fold deals with all the successors of p,
                 universal quantification.*)
              (LTS.fold_succ_e
	         (fun e_p (partial_v_p, partial_l_p, yes_table, no_table) ->
                   let
                       (match_found, v_q, l_q,
                        yes_table,
                        no_table) =
                     (* This fold deals with all the successors of q,
                        existential quantification.*)
                     (LTS.fold_succ_e
		        (fun e_q
                          (partial_match_found,
                           partial_v_q,
                           partial_l_q,
                           yes_table,
                           no_table) ->
                            if (LTS.A.compare (LTS.E.label e_p) (LTS.E.label e_q) <> 0)
                            then
                              (partial_match_found,
                               partial_v_q,
                               partial_l_q,
                               yes_table,
                               no_table)
                            else
                              let
                                  (* this is when we do not switch sides.*)
                                  (v_pp, l_pp, _,
                                   yes_table,
                                   no_table) =
                                (checknkRel
		                   lts1
		                   lts2
		                   (LTS.E.dst e_p)
		                   (LTS.E.dst e_q)
		                   (n)
		                   (k - 1)
                                   yes_table
                                   no_table
		                   rel)
                              in
                              let
                                  (* this is when we switch sides.*)
                                  (v_qq, l_qq, _,
                                   yes_table,
                                   no_table) =
                                if
                                  (n = 0)
                                then
                                  (true,
                                   [],
                                   [],
                                   yes_table,
                                   no_table)
                                else
                                  (checknkRel
		                     lts2
		                     lts1
		                     (LTS.E.dst e_q)
		                     (LTS.E.dst e_p)
		                     (n - 1)
		                     (k - 1)
                                     yes_table
                                     no_table
		                     rel)
                              in
		              (true,
                               partial_v_q || (v_pp && v_qq),
                               partial_l_q @
                                 ((List.map
                                     (fun (n1, k1) -> (n1, k1 + 1))
                                     l_pp) (* one more round. *)
                                  @
                                    (List.map
                                       (fun (n1, k1) -> (n1 + 1, k1 + 1))
                                       l_qq) (* one more round, one more alternation. *)
                                 ) (*this can be
                                     optimised. A LOT. Here,
                                     we should have only the
                                     max value of (n, k) in
                                     cases where a comparison
                                     is possible.*),
                               yes_table,
                               no_table
                              )
		        )
		        lts2
		        q
		        (false, false, [], yes_table, no_table)
	             )
                   in
                   if
                     (not match_found)
                   then
                     (* this is the base case for entry into the
                        no_table. The challenger can perform one move
                        right here which the defender cannot
                        replicate. *)
                     (false,
                      (0, 1) :: l_q,
                      yes_table,
                      no_table) 
                   else
	             (partial_v_p && v_q,
                      partial_l_p @ l_q,  (*this can be
                                            optimised. A LOT. Here,
                                            we should have only the
                                            min value of (n, k) in
                                            cases where a comparison
                                            is possible.*)
                      yes_table,
                      no_table
                     )
	         )
	         lts1
	         p
	         (true, [], yes_table, no_table)
	      )
            in
            if
              v_p
            then
	      (true,
               l_p,
               [],
               yes_table,
               no_table)
            else
	      (false,
               l_p,
               [], (* we need to return a list of pairs of the form (n,
                      k) which denotes the various pairs of values of n
                      and k for which the challenger wins.*)
               remove_entry_yes_table yes_table p q n k,
               List.fold_left
                 (fun no_table (n1, k1) -> add_entry_no_table no_table p q n1 k1)
                 no_table
                 l_p
              )
          )
        )
        in
        let
            () =
          Printf.printf
            "about to pop p = %s, q = %s, n = %s, k = %s, \
 defender_won = %s, \nl_p = [%s], no_table = [%s]\n"
            (LTS.vertex_name p)
            (LTS.vertex_name q)
            (string_of_int n)
            (string_of_int k)
            (match result with
            | (true, _, _, _, _) -> "true"
            | (false, _, _, _, _) -> "false"
            )
            (match result with
            | (_, l_p, _, _, _) ->
              String.concat
                ", "
                (List.map
                   (function (n1, k1) -> "(" ^ (string_of_int n1) ^
                     ", " ^ (string_of_int k1) ^ ")")
                   l_p
                )
            )
            (match result with
            | (_, _, _, _, no_table) ->
              String.concat
                ", "
                (List.map
                   (function (p1, q1, n1, k1) ->
                     "(" ^ (LTS.vertex_name p1) ^ ", " ^ (LTS.vertex_name q1) ^ ", " ^ (string_of_int n1) ^ ", " ^ (string_of_int k1) ^ ")")
                   no_table
                )
            )
        in
        result

     end)

module Test =
  (struct
    module V =
      (struct
        type t = int
        let compare = Pervasives.compare
        let hash = Hashtbl.hash
        let equal = Pervasives.(=)
        let state_name = string_of_int
       end)

    module E1 =
      (struct
        type t = int
        let compare = Pervasives.compare
        let default = 0
        type action = t
        module ActionMap = Map.Make(
          struct
            type t = action
            let compare = Pervasives.compare
          end
        )
        let action_names =
          ActionMap.add 2 "2" (ActionMap.add 1 "1" (ActionMap.add 0 "0" ActionMap.empty))
       end)

    module IntIntLTS1 = LTS_Functor (V) (E1)

    let test93 =
      try
        match
          IntIntLTS1.add_edge IntIntLTS1.empty 0 1
        with
        | _ -> "test93 passed"
      with
      | Invalid_argument _ -> "test93 failed"

    let test94 =
      try
        match
          IntIntLTS1.add_edge_e IntIntLTS1.empty (IntIntLTS1.E.create 0 0 1)
        with
        | _ -> "test94 passed"
      with
      | Invalid_argument _ -> "test94 failed"

    let test95 =
      try
        match
          IntIntLTS1.add_edge_e IntIntLTS1.empty (IntIntLTS1.E.create 0 (-1) 1)
        with
        | _ -> "test95 failed"
      with
      | Invalid_argument _ -> "test95 passed"

    module E2 =
      (struct
        include E1
        let action_names = ActionMap.add 3 "3" (ActionMap.add 2 "2" (ActionMap.add 1 "1" ActionMap.empty))
       end)

    module IntIntLTS2 = LTS_Functor (V) (E2)

    module IntIntLTS1Dot = LTS_Dot_Functor (IntIntLTS1)

    module IntIntLTS2Dot = LTS_Dot_Functor (IntIntLTS2)

    let test96 =
      try
        match
          IntIntLTS2.add_edge IntIntLTS2.empty 0 1
        with
        | g -> "test96 failed"
      with
      | Invalid_argument _ -> "test96 passed"

    let test97 =
      try
        match
          IntIntLTS2.add_edge_e IntIntLTS2.empty (IntIntLTS2.E.create 0 3 1)
        with
        | g -> "test97 passed"
      with
      | Invalid_argument _ -> "test97 failed"

    let l01 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS1.add_edge_e g (IntIntLTS1.E.create src label dst))
        (IntIntLTS1.add_vertex IntIntLTS1.empty 7)
        [(0, 0, 1); (1, 0, 1); (2, 1, 3); (3, 1, 4); (4, 1, 2); (5, 0, 0);
         (6, 0, 3); (5, 1, 6)]

    module F1 = Fernandez_Functor (IntIntLTS1)

    let splitter01 =
      F1.Compound
        ({F1.node_refs =
            List.fold_left
              (function s -> function element ->
                F1.StateSet.add element s
              )
              (F1.StateSet.empty)
              [0; 1; 2; 3; 4]
         (* [4; 3; 2; 1; 0] *) (*This sequence, unsurprisingly,
                                 leads to a test failure. Our
                                 tests are terrible, given that
                                 they use Pervasives.compare for
                                 deep data structures that contain 
                                 sets.*)
         ;
          F1.info = F1.InfoMap.empty
         },
         F1.Simple
           {F1.node_refs =
               List.fold_left
                 (function s -> function element ->
                   F1.StateSet.add element s
                 )
                 (F1.StateSet.empty)
                 [0; 1];
            F1.info = F1.InfoMap.empty
           },
         F1.Compound
           ({F1.node_refs =
               List.fold_left
                 (function s -> function element ->
                   F1.StateSet.add element s
                 )
                 (F1.StateSet.empty)
                 [2; 3; 4];
             F1.info = F1.InfoMap.empty
            },
            F1.Simple
              {F1.node_refs = F1.StateSet.add 2 F1.StateSet.empty;
               F1.info = F1.InfoMap.empty
              },
            F1.Simple
              {F1.node_refs =
                  List.fold_left
                    (function s -> function element ->
                      F1.StateSet.add element s
                    )
                    (F1.StateSet.empty)
                    [4; 3]
              ;
               F1.info = F1.InfoMap.empty
              }
           )
        )

    let splitter02 =
      F1.Compound
        ({F1.node_refs =
            List.fold_left
              (function s -> function element ->
                F1.StateSet.add element s
              )
              (F1.StateSet.empty)
              [0; 1; 2; 3; 4];
          F1.info = F1.InfoMap.empty
         },
         F1.Simple
           {F1.node_refs =
               List.fold_left
                 (function s -> function element ->
                   F1.StateSet.add element s
                 )
                 (F1.StateSet.empty)
                 [0; 1];
            F1.info = F1.InfoMap.empty
           },
         F1.Compound
           ({F1.node_refs =
               List.fold_left
                 (function s -> function element ->
                   F1.StateSet.add element s
                 )
                 (F1.StateSet.empty)
                 [2; 3; 4];
             F1.info = F1.InfoMap.empty
            },
            F1.Simple
              {F1.node_refs = F1.StateSet.add 2 F1.StateSet.empty;
               F1.info = F1.InfoMap.empty
              },
            F1.Compound
              ({F1.node_refs =
                  List.fold_left
                    (function s -> function element ->
                      F1.StateSet.add element s
                    )
                    (F1.StateSet.empty)
                    [3; 4];
                F1.info = F1.InfoMap.empty
               },
               F1.Simple
                 {F1.node_refs = F1.StateSet.add 4 F1.StateSet.empty;
                  F1.info = F1.InfoMap.empty
                 },
               F1.Simple
                 {F1.node_refs = F1.StateSet.add 3 F1.StateSet.empty;
                  F1.info = F1.InfoMap.empty
                 }
              )
           )
        )

    let block01 =
      {F1.node_refs =
          List.fold_left
            (function s -> function element ->
              F1.StateSet.add element s
            )
            (F1.StateSet.empty)
            [5; 6; 7];
       F1.info = F1.InfoMap.empty
      }

    let block02 =
      {F1.node_refs =
          List.fold_left
            (function s -> function element ->
              F1.StateSet.add element s
            )
            (F1.StateSet.empty)
            [5; 7];
       F1.info = F1.InfoMap.empty
      }

    let block03 =
      {F1.node_refs =
          F1.StateSet.add 7 F1.StateSet.empty
      ;
       F1.info = F1.InfoMap.empty
      }

    let block04 =
      {F1.node_refs =
          List.fold_left
            (function s -> function element ->
              F1.StateSet.add element s
            )
            (F1.StateSet.empty)
            [3; 4];
       F1.info = F1.InfoMap.empty
      }

    let block05 =
      {F1.node_refs =
          F1.StateSet.add 4 F1.StateSet.empty
      ;
       F1.info = F1.InfoMap.empty
      }

    let block06 =
      {F1.node_refs =
          F1.StateSet.add 3 F1.StateSet.empty
      ;
       F1.info = F1.InfoMap.empty
      }

    let test98 =
      if
        F1.two_way_update_tree splitter01 block01 block02 block03 =
        (splitter01, false)
      then
        "test98 passed"
      else
        "test98 failed"

    let test99 =
      if
        F1.two_way_update_tree splitter01 block04 block05 block06 =
        (splitter02, true)
      then
        "test99 passed"
      else
        "test99 failed"

    let l02 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS1.add_edge_e g (IntIntLTS1.E.create src label dst))
        IntIntLTS1.empty
        [(0, 1, 1); (0, 1, 2); (1, 0, 2); (2, 0, 1)]

    let block08 = F1.get_block_from_node_refs l02
      (List.fold_left
         (fun s elem -> F1.StateSet.add elem s)
         F1.StateSet.empty
         [1; 2]
      )

    let test100 =
      if
        F1.InfoMap.find (1, 0) block08.F1.info = 2
      then
        "test100 passed"
      else
        "test100 failed, cardinal = " ^
          (string_of_int
             (F1.InfoMap.cardinal block08.F1.info)
          )

    let test101 =
      if
        F1.InfoMap.find (0, 1) block08.F1.info = 1
      then
        "test101 passed"
      else
        "test101 failed, cardinal = " ^
          (string_of_int
             (F1.InfoMap.cardinal block08.F1.info)
          )

    let test102 =
      if
        F1.InfoMap.find (1, 1) block08.F1.info = 0
      then
        "test102 passed"
      else
        "test102 failed"

    let block09 = F1.get_block_from_node_refs l02
      (List.fold_left
         (fun s elem -> F1.StateSet.add elem s)
         F1.StateSet.empty
         [0; 1; 2]
      )

    let test103 =
      if
        match
          F1.simple_split_block_on_action l02 block09 block09 0
        with
        | (x1, x2) ->
          (F1.StateSet.equal
             x1.F1.node_refs
             (F1.StateSet.add 2 (F1.StateSet.add 1 F1.StateSet.empty))
          ) && (F1.StateSet.equal
                  x2.F1.node_refs
                  (F1.StateSet.add 0 F1.StateSet.empty)
           )
          then
            "test103 passed"
          else
            "test103 failed"

    let test104 =
      if
        match
          F1.simple_split_block_on_action l02 block09 block09 1
        with
        | (x1, x2) ->
          (F1.StateSet.equal
             x1.F1.node_refs
             (F1.StateSet.add 0 F1.StateSet.empty)
          ) && (F1.StateSet.equal
                  x2.F1.node_refs
                  (F1.StateSet.add 2 (F1.StateSet.add 1 F1.StateSet.empty))
           )
          then
            "test104 passed"
          else
            "test104 failed"

    let test105 =
      if
        match
          F1.simple_split_block_on_action l02 block09 block09 2
        with
        | (x1, x2) ->
          (F1.StateSet.equal
             x1.F1.node_refs
             F1.StateSet.empty
          ) && (F1.StateSet.equal
                  x2.F1.node_refs
                  (F1.StateSet.add 2 (F1.StateSet.add 1 (F1.StateSet.add 0 F1.StateSet.empty)))
           )
          then
            "test105 passed"
          else
            "test105 failed"

    let block10 = F1.get_block_from_node_refs l01
      (List.fold_left
         (fun s elem -> F1.StateSet.add elem s)
         F1.StateSet.empty
         [0; 1; 2; 3; 4; 5; 6; 7]
      )

    let block11 = F1.get_block_from_node_refs l01
      (List.fold_left
         (fun s elem -> F1.StateSet.add elem s)
         F1.StateSet.empty
         [0; 1]
      )

    let block12 = F1.get_block_from_node_refs l01
      (List.fold_left
         (fun s elem -> F1.StateSet.add elem s)
         F1.StateSet.empty
         [2; 3; 4; 5; 6; 7]
      )

    module E3 =
      (struct
        include E1
        let action_names =
          ActionMap.add 3 "3"
            (ActionMap.add 2 "2"
               (ActionMap.add 1 "1"
                  (ActionMap.add 0 "0"
                     ActionMap.empty)))
       end)

    module IntIntLTS3 = LTS_Functor (V) (E3)

    let l03 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS3.add_edge_e g (IntIntLTS3.E.create src label dst))
        IntIntLTS3.empty
        [(0, 0, 1);
         (0, 0, 2);
         (1, 1, 3);
         (2, 1, 4);
         (2, 1, 5);
         (3, 2, 6);
         (3, 2, 7);
         (4, 2, 8);
         (5, 2, 9);
         (5, 2, 10);
         (6, 3, 11);
         (8, 3, 12);
         (9, 3, 13)]

    let l04 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS3.add_edge_e g (IntIntLTS3.E.create src label dst))
        IntIntLTS3.empty
        [(14, 0, 15);
         (15, 1, 16);
         (15, 1, 17);
         (16, 2, 18);
         (17, 2, 19);
         (17, 2, 20);
         (18, 3, 21);
         (19, 3, 22)]

    let l05 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS3.add_edge_e g (IntIntLTS3.E.create src label dst))
        IntIntLTS3.empty
        [(23, 0, 23);
         (23, 0, 24)]

    let l06 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS3.add_edge_e g (IntIntLTS3.E.create src label dst))
        IntIntLTS3.empty
        [(25, 0, 25)]

    let l07 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS3.add_edge_e g (IntIntLTS3.E.create src label dst))
        IntIntLTS3.empty
        [(26, 0, 27);
         (26, 0, 28);
         (27, 0, 29);
         (27, 1, 30);
         (28, 0, 30);
         (29, 0, 26);
         (30, 0, 26)]

    let l08 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS3.add_edge_e g (IntIntLTS3.E.create src label dst))
        IntIntLTS3.empty
        [(31, 0, 32);
         (31, 0, 34);
         (32, 0, 33);
         (32, 1, 33);
         (33, 0, 31);
         (34, 0, 35);
         (35, 0, 31)]

    let l09 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS3.add_edge_e g (IntIntLTS3.E.create src label dst))
        IntIntLTS3.empty
        [(26, 0, 28);
         (26, 0, 27);
         (28, 0, 29);
         (28, 1, 30);
         (27, 0, 30);
         (29, 0, 26);
         (30, 0, 26)]

    let l10 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS3.add_edge_e g (IntIntLTS3.E.create src label dst))
        IntIntLTS3.empty
        [(31, 0, 32);
         (31, 0, 34);
         (32, 0, 33);
         (32, 1, 33);
         (33, 0, 31);
         (34, 0, 35);
         (35, 0, 31);
         (31, 0, 36);
         (36, 0, 37);
         (37, 0, 31)]

    let l11 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS3.add_edge_e g (IntIntLTS3.E.create src label dst))
        IntIntLTS3.empty
        [(38, 0, 38)]

    let l12 =
      List.fold_left
        (fun g (src, label, dst) -> IntIntLTS3.add_edge_e g (IntIntLTS3.E.create src label dst))
        IntIntLTS3.empty
        [(39, 0, 39)]

    let () =
      IntIntLTS3.iter_vertex
        (function v ->
          Printf.printf
            "v = %s\n"
            (string_of_int v)
        )
        l04

    module IntIntLTS3NK_Rel = NK_Rel (IntIntLTS3)

    let test120 =
      match
        IntIntLTS3NK_Rel.checknkRel
	  l03
	  l04
	  0
	  14
	  3
	  4
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
	  ()
      with
      | (false, _, _, _, _) -> "test120 passed"
      | (true, _, _, _, _) -> "test120 failed"

    (* Shibashis: I assume that true means the defender has won
       and the relation holds when the challenger starts with l03 in
       the first round *)
        
    let test121 =  (* Shibashis: Challenger chooses now l04 in the
    first round *)
      match
        IntIntLTS3NK_Rel.checknkRel
	  l04
	  l03
	  14
	  0
	  3
	  4
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
	  ()
      with
      | (true, _, _, _, _) -> "test121 passed"
      | (false, _, _, _, _) -> "test121 failed"

    let test122 =
      match
        IntIntLTS3NK_Rel.checknkRel
	  l03
	  l04
	  0
	  14
	  2
	  4
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
	  ()
      with
      | (true, _, _, _, _) -> "test122 passed"
      | (false, _, _, _, _) -> "test122 failed"

    (* Shibashis: I assume that true means the defender has won
       and the relation holds when the challenger starts with l03 in
       the first round *)
        
    let test123 =  (* Shibashis: Challenger chooses now l04 in the
    first round *)
      match
        IntIntLTS3NK_Rel.checknkRel
	  l04
	  l03
	  14
	  0
	  2
	  4
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
	  ()
      with
      | (true, _, _, _, _) -> "test123 passed"
      | (false, _, _, _, _) -> "test123 failed"

    let test124 =
      match
        IntIntLTS3NK_Rel.checknkRel
	  l03
	  l04
	  0
	  14
	  3
	  6
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
	  ()
      with
      | (false, _, _, _, _) -> "test124 passed"
      | (true, _, _, _, _) -> "test124 failed"

    (* Shibashis: I assume that true means the defender has won
       and the relation holds when the challenger starts with l03 in
       the first round *)
        
    let test125 =  (* Shibashis: Challenger chooses now l04 in the
    first round *)
      match
        IntIntLTS3NK_Rel.checknkRel
	  l04
	  l03
	  14
	  0
	  3
	  6
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
	  ()
      with
      | (true, _, _, _, _) -> "test125 passed"
      | (false, _, _, _, _) -> "test125 failed"

    let test126 =
      match
        IntIntLTS3NK_Rel.checknkRel
	  l03
	  l04
	  0
	  14
	  2
	  6
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
	  ()
      with
      | (true, _, _, _, _) -> "test126 passed"
      | (false, _, _, _, _) -> "test126 failed"

    (* Shibashis: I assume that true means the defender has won
       and the relation holds when the challenger starts with l03 in
       the first round *)
        
    let test127 =  (* Shibashis: Challenger chooses now l04 in the
    first round *)
      match
        IntIntLTS3NK_Rel.checknkRel
	  l04
	  l03
	  14
	  0
	  2
	  6
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
	  ()
      with
      | (true, _, _, _, _) -> "test127 passed"
      | (false, _, _, _, _) -> "test127 failed"

    let test128 =
      match
        IntIntLTS3NK_Rel.checknkRel
	  l05
	  l06
	  23
	  25
	  1
	  2
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
	  ()
      with
      | (false, _, _, _, _) -> "test128 passed"
      | (true, _, _, _, _) -> "test128 failed"

    let test129 =
      match
        IntIntLTS3NK_Rel.checknkRel
          l05
          l06
          23
          25
          0
          2
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
          ()
      with
      | (true, _, _, _, _) -> "test129 passed"
      | (false, _, _, _, _) -> "test129 failed"

    let test130 =
      match
        IntIntLTS3NK_Rel.checknkRel
          l05
          l06
          23
          25
          0
          5
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
          ()
      with
      | (true, _, _, _, _) -> "test130 passed"
      | (false, _, _, _, _) -> "test130 failed"

    let test131 =
      match
        IntIntLTS3NK_Rel.checknkRel
          l06
          l05
          25
          23
          1
          2
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
          ()
      with
      | (true, _, _, _, _) -> "test131 passed"
      | (false, _, _, _, _) -> "test131 failed"

    let test132 =
      match
        IntIntLTS3NK_Rel.checknkRel
          l06
          l05
          25
          23
          2
          3
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
          ()
      with
      | (false, _, _, _, _) -> "test132 passed"
      | (true, _, _, _, _) -> "test132 failed"

    let test133 =
      match
        IntIntLTS3NK_Rel.checknkRel
          l06
          l05
          25
          23
          4
          10
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
          ()
      with
      | (false, _, _, _, _) -> "test133 passed"
      | (true, _, _, _, _) -> "test133 failed"

    let test134 =
      match
        IntIntLTS3NK_Rel.checknkRel
          l07
          l08
          26
          31
          10
          20
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
          ()
      with
      | (true, _, _, _, _) -> "test134 passed"
      | (false, _, _, _, _) -> "test134 failed"

    let test135 =
      match
        IntIntLTS3NK_Rel.checknkRel
          l08
          l07
          32
          28
          0
          1
          (IntIntLTS3NK_Rel.create_yes_table ())
          (IntIntLTS3NK_Rel.create_no_table ())
          ()
      with
      | (false, _, _, _, _) -> "test135 passed"
      | (true, _, _, _, _) -> "test135 failed"

    let f01 =
      IntIntLTS3NK_Rel.DIAMOND
        (0,
         IntIntLTS3NK_Rel.BOX
           (1,
            IntIntLTS3NK_Rel.DIAMOND
              (2,
               IntIntLTS3NK_Rel.AND [])))

    let f02 =
      IntIntLTS3NK_Rel.BOX
        (0,
         IntIntLTS3NK_Rel.DIAMOND
           (1,
            IntIntLTS3NK_Rel.BOX
              (2,
               IntIntLTS3NK_Rel.OR [])))

    let test136 =
      if
        (IntIntLTS3NK_Rel.negation f01 = f02)
      then
        "test136 passed"
      else
        "test136 failed"

    let test137 =
      if
        (IntIntLTS3NK_Rel.negation f02 = f01)
      then
        "test137 passed"
      else
        "test137 failed"

    let f03 =
      (let
          sf01 =
         IntIntLTS3NK_Rel.DIAMOND
           (1,
            IntIntLTS3NK_Rel.BOX
              (2,
               IntIntLTS3NK_Rel.DIAMOND
                 (3,
                  IntIntLTS3NK_Rel.AND [])))
       in
       let
           sf02 =
         IntIntLTS3NK_Rel.AND
           [IntIntLTS3NK_Rel.BOX (4, IntIntLTS3NK_Rel.OR []);
            IntIntLTS3NK_Rel.BOX (5, IntIntLTS3NK_Rel.OR [])]
       in
       IntIntLTS3NK_Rel.BOX
         (0,
          IntIntLTS3NK_Rel.OR
            [sf01;
             IntIntLTS3NK_Rel.BOX
               (1, sf02)]))

    let f04 =
      (let
          sf01 =
         IntIntLTS3NK_Rel.BOX
           (1,
            IntIntLTS3NK_Rel.DIAMOND
              (2,
               IntIntLTS3NK_Rel.BOX
                 (3,
                  IntIntLTS3NK_Rel.OR [])))
       in
       let
           sf02 =
         IntIntLTS3NK_Rel.OR
           [IntIntLTS3NK_Rel.DIAMOND (4, IntIntLTS3NK_Rel.AND []);
            IntIntLTS3NK_Rel.DIAMOND (5, IntIntLTS3NK_Rel.AND [])]
       in
       IntIntLTS3NK_Rel.DIAMOND
         (0,
          IntIntLTS3NK_Rel.AND
            [sf01;
             IntIntLTS3NK_Rel.DIAMOND
               (1, sf02)]))

    let test138 =
      if
        (IntIntLTS3NK_Rel.negation f03 = f04)
      then
        "test138 passed"
      else
        "test138 failed"

    let test139 =
      if
        (IntIntLTS3NK_Rel.negation f04 = f03)
      then
        "test139 passed"
      else
        "test139 failed"

      end)

