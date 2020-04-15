(*
 * mew.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew.
 *)

module Concurrent = Concurrent
module Modal = Modal
module Mode = Mode
module Key = Key

module Make (Modal:Modal.S) (Concurrent:Concurrent.S) =
struct

  module Key = Modal.Key
  module Mode = Modal.Mode
  module MsgBox = Concurrent.MsgBox
  module Thread = Concurrent.Thread

  let (>>=)= Thread.bind

  class edit state= object(self)
    val i= MsgBox.create ()
    val o: Key.t MsgBox.t= MsgBox.create ()
    val mutable curr_mode: Mode.t= let _, mode= state#default_mode in mode
    method keyin (key:Key.t)= MsgBox.put i key

    method i= i
    method o= o

    method getMode= curr_mode
    method setMode mode= curr_mode <- Mode.Modes.find mode state#modes
    method timeout= match Mode.timeout self#getMode with
      | Some timeout-> timeout
      | None-> state#timeout
    method bindings= Mode.bindings self#getMode

    initializer
      let rec get_key sources=
        match sources with
        | []-> MsgBox.get i >>= fun key-> Thread.return (key, sources)
        | source::tl->
          match Queue.take source with
          | key-> Thread.return (key, sources)
          | exception Queue.Empty-> get_key tl
      in
      let output_seq o seq=
        let rec output_seq ()=
          match Queue.take seq with
          | key-> MsgBox.put o key >>= output_seq
          | exception Queue.Empty-> Thread.return ()
        in
        output_seq ()
      in
      let perform action sources=
        match action with
        | Mode.Switch name-> self#setMode name;
          sources
        | Key key->
          let seq= Queue.create() in
          Queue.add key seq;
          seq::sources
        | KeySeq keyseq-> keyseq::sources
        | Custom f-> f (); sources
      in
      let rec listen sources mem_key last node=
        match node with
        | Some node->
          Thread.pick
            [ (Thread.sleep self#timeout >>=
                fun ()-> Thread.return None);
              get_key sources >>= fun (key, sources)->
                Thread.return (Some (key, sources))
              ]
          >>= (function
            | Some (key, sources)->
              Queue.add key mem_key;
              try_matching sources mem_key last node key
            | None-> skip_matching sources mem_key last)
        | None-> let node= self#bindings in
          get_key sources >>= fun (key, sources)->
          Queue.add key mem_key;
          try_matching sources mem_key last node key
      and try_matching sources mem_key last node key=
        match Mode.KeyTrie.sub node [key] with
        | Some node->
          let last=
            match Mode.KeyTrie.get node [] with
            | Some action->
              Some (Queue.copy mem_key, action)
            | None-> last
          in
          if Mode.KeyTrie.is_leaf node then
            skip_matching sources mem_key last
          else
            listen sources mem_key last (Some node)
        | None->
          skip_matching sources mem_key last
      and skip_matching sources mem_key last=
        match last with
        | Some (seq, action)->
          Utils.Queue.drop (Queue.length seq) mem_key;
          let sources= perform action sources in
          listen (mem_key::sources) (Queue.create ()) None None
        | None->
          output_seq o mem_key >>= fun ()->
          listen sources (Queue.create()) None None
      in
      Thread.async (fun ()-> listen [] (Queue.create ()) None None)
  end

  class state modes= object(self)
    val mutable timeout= 1.
    val mutable default_mode= Modal.Mode.default_mode modes

    method edit= new edit self
    method modes= modes
    method default_mode= default_mode
    method timeout= timeout
  end
end

