module CleverRake.XnaUtils.Error

exception InternalError of string

let failInternalWith msg = raise <| InternalError(msg)

exception UserError of obj option * string

let failUserErrorWithData data msg = raise <| UserError(Some data, msg)
let failUserErrorWith msg = raise <| UserError(None, msg)