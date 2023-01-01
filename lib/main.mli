val emitproc : out_channel -> Frame.frag -> unit
val withOpenFile : string -> (out_channel -> unit) -> unit 
val compile : string -> unit