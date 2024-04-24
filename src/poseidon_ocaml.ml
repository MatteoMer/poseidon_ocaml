module Field = Ff.MakeFp (
    struct
        (* BN254 Field *)
        let prime_order = Z.of_string_base 16 "2523648240000001BA344D80000000086121000000000013A700000000000013" 
    end
)

module Poseidon = struct

    type mds_matrix = Field.t list list
    type constants = Field.t list
    type inputs = Field.t list

    type parameters = {
        t: int;
        alpha: Z.t;
        r_f: int;
        r_p: int;
        m: mds_matrix;
        c: constants;
    }

    let s_box inputs alpha = 
        List.map (fun x -> Field.pow x alpha) inputs

    let round_constant inputs constants = 
        List.map2 (fun x y -> Field.add x y) inputs constants

    let dot_product vec col = 
        List.fold_left2 (fun acc x y -> Field.add acc (Field.mul x y)) Field.zero vec col

    let cols matrix =
      let rec extract_col matrix col_idx =
        List.fold_right (fun row acc -> (List.nth row col_idx) :: acc) matrix []
      in
      let num_cols = List.length (List.hd matrix) in
      List.init num_cols (extract_col matrix)

    let mds_mul i mds_matrix = 
        let columns = cols mds_matrix in 
        List.map (dot_product i) columns

    let rec full_round (i: inputs) (p: parameters) rounds = 
        if rounds <= 0 then
            i
        else
            let add_constants = round_constant i p.c in
            let s_boxed = s_box add_constants p.alpha in
            let matrix = mds_mul s_boxed p.m in
            full_round matrix p (rounds - 1)

    let hash (i: inputs)(p: parameters) = 
        let _ = assert(List.length i=List.length p.c) in (* Check that the constants size is the same as inputs size *) 
        let result_vec = full_round i p p.r_f in
        result_vec

end
