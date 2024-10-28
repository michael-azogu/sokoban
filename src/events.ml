module Events : sig
  val register : unit React.event list -> unit
  val on : 'a React.event -> run:('a -> 'b) -> 'b React.event
end = struct
  let on event ~run = React.E.map run event

  let listeners : 'a React.event list ref = ref []

  let register l =
    !listeners |> List.iter (fun e -> React.E.stop e) ;
    listeners := l
end
