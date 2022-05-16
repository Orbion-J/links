let _ =
object (self)
    inherit SugarTraversals.map as super

    method! row_var = function
        | EffectApplication ()
