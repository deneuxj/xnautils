module CleverRake.XnaUtils.Trinomial

/// Solve Ax^2 + Bx + C = 0
let solve A B C =
    let delta = B * B - 4.0 * A * C
    if float delta > 0.0 then
        let d = sqrt delta
        let a = 2.0 * A
        let r0 = (-B - d)/a
        let r1 = (-B + d)/a
        [r0;r1]
    elif float delta = 0.0 then
        [0.5 * -B/A]
    else
        []