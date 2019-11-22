(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

let scan_AD: string option ref = ref None

let use_AD = ref true

let verbose = ref false

type utility_function = ROC_AUC
                      | PR_AUC

let optim_target = ref ROC_AUC (* the default *)
