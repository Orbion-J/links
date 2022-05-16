open Sugartypes

    let find_name name =
    object (self)
        inherit SugarTraversals.predicate as super

        val pred = false

        method satisfied = pred

        method! row_var = function
            | Datatype.EffectApplication(name', _) -> {< pred = (name' = name) >}
            | x -> super#row_var x
    end

    let replace name var =
    object (self)
        inherit SugarTraversals.map as super

        method! row_var = function
            | Datatype.EffectApplication(name', _) when name' = name -> Datatype.Open var
            | x -> super#row_var x
    end

    let visitor =
    object (self)
        inherit SugarTraversals.map as super

        method! effectnamenode (name, args, body') =
            let body, _ = body' in
            if ((find_name name)#row body)#satisfied then
                let var = SugarTypeVar.mk_unresolved ("µµ" ^ name) None `Rigid in
                let body = (replace name var)#row body in
                let body = ([], Datatype.Recursive(var, body)) in
                (name, args, (body, None))
            else
                (name, args, body')
    end

let program p = visitor#program p

let sentence = function
  | Definitions bs ->
      let bs' = visitor#list (fun o b -> o#binding b) bs in
        Definitions bs'
  | Expression  p  -> Expression p
  | Directive   d  -> Directive d

module Untyped = struct
    open Transform.Untyped

    let name = "recursive_effect_aliases"

    let program state program' =
        let program' = program program' in
        return state program'

    let sentence state sentence' =
        let sentence' = sentence sentence' in
        return state sentence'
end
