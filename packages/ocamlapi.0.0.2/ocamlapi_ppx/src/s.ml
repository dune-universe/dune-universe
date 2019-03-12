module type Component = sig
    val name: string
    val register: unit -> unit 
end
