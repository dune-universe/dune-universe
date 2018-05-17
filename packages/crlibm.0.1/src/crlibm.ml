
module type S = sig
  val exp : float -> float
  val expm1 : float -> float
  val log : float -> float
  val log1p : float -> float
  val cos : float -> float
  val sin : float -> float
  val tan : float -> float
  val cospi : float -> float
  val sinpi : float -> float
  val tanpi : float -> float
  val asin : float -> float
  val acos : float -> float
  val atan : float -> float
  val asinpi : float -> float
  val acospi : float -> float
  val atanpi : float -> float
  val cosh : float -> float
  val sinh : float -> float
  val log2 : float -> float
  val log10 : float -> float
end

(* Order of the definitions is the same as in crlibm/crlibm.h *)
external exp : float -> float = "exp_rn_bc" "exp_rn" [@@unboxed]
external log : float -> float = "log_rn_bc" "log_rn" [@@unboxed]
external cos : float -> float = "cos_rn_bc" "cos_rn" [@@unboxed]
external sin : float -> float = "sin_rn_bc" "sin_rn" [@@unboxed]
external tan : float -> float = "tan_rn_bc" "tan_rn" [@@unboxed]
external cospi : float -> float = "cospi_rn_bc" "cospi_rn" [@@unboxed]
external sinpi : float -> float = "sinpi_rn_bc" "sinpi_rn" [@@unboxed]
external tanpi : float -> float = "tanpi_rn_bc" "tanpi_rn" [@@unboxed]

external atan : float -> float = "atan_rn_bc" "atan_rn" [@@unboxed]
external atanpi : float -> float = "atanpi_rn_bc" "atanpi_rn" [@@unboxed]
external cosh : float -> float = "cosh_rn_bc" "cosh_rn" [@@unboxed]
external sinh : float -> float = "sinh_rn_bc" "sinh_rn" [@@unboxed]
external log2 : float -> float = "log2_rn_bc" "log2_rn" [@@unboxed]
external log10 : float -> float = "log10_rn_bc" "log10_rn" [@@unboxed]
external asin : float -> float = "asin_rn_bc" "asin_rn" [@@unboxed]
external acos : float -> float = "acos_rn_bc" "acos_rn" [@@unboxed]
external asinpi : float -> float = "asinpi_rn_bc" "asinpi_rn" [@@unboxed]
external acospi : float -> float = "acospi_rn_bc" "acospi_rn" [@@unboxed]
external expm1 : float -> float = "expm1_rn_bc" "expm1_rn" [@@unboxed]
external log1p : float -> float = "log1p_rn_bc" "log1p_rn" [@@unboxed]

external pow : float -> float -> float = "pow_rn_bc" "pow_rn" [@@unboxed]

module Low = struct
  external exp : float -> float = "exp_rd_bc" "exp_rd" [@@unboxed]
  external log : float -> float = "log_rd_bc" "log_rd" [@@unboxed]
  external cos : float -> float = "cos_rd_bc" "cos_rd" [@@unboxed]
  external sin : float -> float = "sin_rd_bc" "sin_rd" [@@unboxed]
  external tan : float -> float = "tan_rd_bc" "tan_rd" [@@unboxed]
  external cospi : float -> float = "cospi_rd_bc" "cospi_rd" [@@unboxed]
  external sinpi : float -> float = "sinpi_rd_bc" "sinpi_rd" [@@unboxed]
  external tanpi : float -> float = "tanpi_rd_bc" "tanpi_rd" [@@unboxed]

  external atan : float -> float = "atan_rd_bc" "atan_rd" [@@unboxed]
  external atanpi : float -> float = "atanpi_rd_bc" "atanpi_rd" [@@unboxed]
  external cosh : float -> float = "cosh_rd_bc" "cosh_rd" [@@unboxed]
  external sinh : float -> float = "sinh_rd_bc" "sinh_rd" [@@unboxed]
  external log2 : float -> float = "log2_rd_bc" "log2_rd" [@@unboxed]
  external log10 : float -> float = "log10_rd_bc" "log10_rd" [@@unboxed]
  external asin : float -> float = "asin_rd_bc" "asin_rd" [@@unboxed]
  external acos : float -> float = "acos_rd_bc" "acos_rd" [@@unboxed]
  external asinpi : float -> float = "asinpi_rd_bc" "asinpi_rd" [@@unboxed]
  external acospi : float -> float = "acospi_rd_bc" "acospi_rd" [@@unboxed]
  external expm1 : float -> float = "expm1_rd_bc" "expm1_rd" [@@unboxed]
  external log1p : float -> float = "log1p_rd_bc" "log1p_rd" [@@unboxed]
end

module High = struct
  external exp : float -> float = "exp_ru_bc" "exp_ru" [@@unboxed]
  external log : float -> float = "log_ru_bc" "log_ru" [@@unboxed]
  external cos : float -> float = "cos_ru_bc" "cos_ru" [@@unboxed]
  external sin : float -> float = "sin_ru_bc" "sin_ru" [@@unboxed]
  external tan : float -> float = "tan_ru_bc" "tan_ru" [@@unboxed]
  external cospi : float -> float = "cospi_ru_bc" "cospi_ru" [@@unboxed]
  external sinpi : float -> float = "sinpi_ru_bc" "sinpi_ru" [@@unboxed]
  external tanpi : float -> float = "tanpi_ru_bc" "tanpi_ru" [@@unboxed]

  external atan : float -> float = "atan_ru_bc" "atan_ru" [@@unboxed]
  external atanpi : float -> float = "atanpi_ru_bc" "atanpi_ru" [@@unboxed]
  external cosh : float -> float = "cosh_ru_bc" "cosh_ru" [@@unboxed]
  external sinh : float -> float = "sinh_ru_bc" "sinh_ru" [@@unboxed]
  external log2 : float -> float = "log2_ru_bc" "log2_ru" [@@unboxed]
  external log10 : float -> float = "log10_ru_bc" "log10_ru" [@@unboxed]
  external asin : float -> float = "asin_ru_bc" "asin_ru" [@@unboxed]
  external acos : float -> float = "acos_ru_bc" "acos_ru" [@@unboxed]
  external asinpi : float -> float = "asinpi_ru_bc" "asinpi_ru" [@@unboxed]
  external acospi : float -> float = "acospi_ru_bc" "acospi_ru" [@@unboxed]
  external expm1 : float -> float = "expm1_ru_bc" "expm1_ru" [@@unboxed]
  external log1p : float -> float = "log1p_ru_bc" "log1p_ru" [@@unboxed]
end

module Zero = struct
  external exp : float -> float = "exp_rd_bc" "exp_rd" [@@unboxed]
  external log : float -> float = "log_rz_bc" "log_rz" [@@unboxed]
  external cos : float -> float = "cos_rz_bc" "cos_rz" [@@unboxed]
  external sin : float -> float = "sin_rz_bc" "sin_rz" [@@unboxed]
  external tan : float -> float = "tan_rz_bc" "tan_rz" [@@unboxed]
  external cospi : float -> float = "cospi_rz_bc" "cospi_rz" [@@unboxed]
  external sinpi : float -> float = "sinpi_rz_bc" "sinpi_rz" [@@unboxed]
  external tanpi : float -> float = "tanpi_rz_bc" "tanpi_rz" [@@unboxed]

  external atan : float -> float = "atan_rz_bc" "atan_rz" [@@unboxed]
  external atanpi : float -> float = "atanpi_rz_bc" "atanpi_rz" [@@unboxed]
  external cosh : float -> float = "cosh_rz_bc" "cosh_rz" [@@unboxed]
  external sinh : float -> float = "sinh_rz_bc" "sinh_rz" [@@unboxed]
  external log2 : float -> float = "log2_rz_bc" "log2_rz" [@@unboxed]
  external log10 : float -> float = "log10_rz_bc" "log10_rz" [@@unboxed]
  external asin : float -> float = "asin_rz_bc" "asin_rz" [@@unboxed]
  external acos : float -> float = "acos_rd_bc" "acos_rd" [@@unboxed]
  external asinpi : float -> float = "asinpi_rz_bc" "asinpi_rz" [@@unboxed]
  external acospi : float -> float = "acospi_rd_bc" "acospi_rd" [@@unboxed]
  external expm1 : float -> float = "expm1_rz_bc" "expm1_rz" [@@unboxed]
  external log1p : float -> float = "log1p_rz_bc" "log1p_rz" [@@unboxed]
end
