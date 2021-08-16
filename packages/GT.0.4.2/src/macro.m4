changequote([[,]])

define(GENERIFY, [[
type proto$1 = $1
type $1 = proto$1

class virtual ['inh, 'self, 'syn] $1_t =
  object
    method virtual t_$1 : 'inh -> $1 -> 'syn
  end

class ['extra] html_$1_t _fself =
  object
    inherit [unit, 'extra, HTML.viewer] $1_t
    method t_$1 inh x = HTML.string (string_of_$1 x)
  end
class ['extra] show_$1_t _fself =
  object
    inherit [unit, 'extra, string] $1_t
    method t_$1 inh x = string_of_$1 x
  end
class ['extra] enum_$1_t _fself =
  object
    inherit [unit, 'extra, int] $1_t
    method t_$1 inh x = 0
  end
class ['extra] fmt_$1_t _fself =
  object
    inherit [Format.formatter, 'extra, unit] $1_t
    method t_$1 fmt x = Format.pp_print_$1 fmt x
  end
class ['syn, 'extra] foldl_$1_t _fself =
  object
    inherit ['syn, 'extra, 'syn] $1_t
    method t_$1 s _ = s
  end
class ['syn, 'extra] foldr_$1_t _fself =
  object
    inherit ['syn, 'extra, 'syn] $1_t
    method t_$1 s _ = s
  end
class ['extra] eq_$1_t _fself =
  object
    inherit [$1, 'extra, 'bool] $1_t
    method t_$1 inh x = x = inh
  end
class ['extra] compare_$1_t _fself =
  object
    inherit [$1, 'extra, 'comparison] $1_t
    method t_$1 inh x = compare_primitive inh x
  end

class ['extra, 'syn] gmap_$1_t _fself =
  object
    constraint 'syn = $1
    inherit [unit, 'extra, 'syn'] $1_t
    method t_$1 _ x = x
  end
class [ 'extra, 'syn, 'env] eval_$1_t _fself =
  object
    constraint 'syn = $1
    inherit ['env, 'extra, '$1] $1_t
    method t_$1 inh x = x
  end
class [ 'extra, 'syn, 'env] stateful_$1_t _fself =
  object
    constraint 'syn = $1
    inherit ['env, 'extra, 'env * $1] $1_t
    method t_$1 inh x = (inh,x)
  end

let gcata_$1 tr inh x = tr#t_$1 inh x

let $1 : (('inh, $1, 'syn) # $1_t -> 'inh -> $1 -> 'syn,
          < show    : $1 -> string;
            enum    : $1 -> int;
            html    : $1 -> HTML.viewer;
            fmt     : Format.formatter -> $1 -> unit;
            compare : $1 -> $1 -> comparison;
            eq      : $1 -> $1 -> bool;
            gmap    : $1 -> $1;
            eval    : 'env -> $1 -> $1;
            stateful: 'env -> $1 -> 'env * $1;
            foldl   : 'a -> $1 -> 'a;
            foldr   : 'a -> $1 -> 'a >,
            (('inh -> $1 -> 'syn) -> ('inh, $1, 'syn) $1_t) -> 'inh -> $1 -> 'syn) t =
  {gcata = gcata_$1;
   fix = (fun c -> transform_gc gcata_$1 c);
   plugins =
      object
        method show    = transform_gc gcata_$1 (new show_$1_t    ) ()
        method enum    = transform_gc gcata_$1 (new enum_$1_t    ) ()
        method gmap    = transform_gc gcata_$1 (new gmap_$1_t    ) ()
        method html    = transform_gc gcata_$1 (new html_$1_t    ) ()
        method fmt     = transform_gc gcata_$1 (new fmt_$1_t     )
        method compare = transform_gc gcata_$1 (new compare_$1_t )
        method eq      = transform_gc gcata_$1 (new eq_$1_t      )
        method eval    = transform_gc gcata_$1 (new eval_$1_t    )
        method stateful= transform_gc gcata_$1 (new stateful_$1_t)
        method foldl   = transform_gc gcata_$1 (new foldl_$1_t   )
        method foldr   = transform_gc gcata_$1 (new foldr_$1_t   )
      end
  }
]])
