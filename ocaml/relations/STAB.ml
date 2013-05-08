open ZVG_modules

let nodes_to_other_nodes
    l1
    l2
    z1
    z2
    = 
  (
    List.concat
      (List.map
         (function a ->
           Printf.printf "a = %s\n" (ZVGQuotient2.expand_action l1 a);
           let
               lz4 = out_adjacency l2 z2 a
           in
           List.map
             (function z3 ->
               Printf.printf "z3 = %s\n" (ZVGQuotient2.node_name l1 z3);
               (z3, a, lz4)
             )
             (out_adjacency l1 z1 a)
         )
         (ZVGQuotient2.actions l1)
      )
      ,
    List.concat
      (List.map
         (function a ->
           Printf.printf "a = %s\n" (ZVGQuotient2.expand_action l2 a);
           let
               lz3 = out_adjacency l1 z1 a
           in
           List.map
             (function z4 ->
               Printf.printf "z4 = %s\n" (ZVGQuotient2.node_name l2 z4);
               (lz3, a, z4))
             (out_adjacency l2 z2 a)
         )
         (ZVGQuotient2.actions l2)
      )
  )
