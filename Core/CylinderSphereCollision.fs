module CleverRake.XnaUtils.Collisions.CylinderSphere

open CleverRake.XnaUtils.Units

/// Compute the times at which a sphere enters and leaves a cylinder.
let computeCollisionTimes
    ((cyl_x : float<m>, cyl_y, cyl_z) as cyl_pos) // Position of the base of the cylinder
    ((Ax : float<m>, Ay, Az) as A) // Vector from the base to the top of the cylinder
    cyl_r // Radius of the cylinder
    ((vcx : float<m/s>, vcy, vcz) as vc) // Velocity of the cylinder
    ((sph_x : float<m>, sph_y, sph_z) as sph_pos) // Position of the center of the sphere
    sph_r // Radius of the sphere
    ((vsx : float<m/s>, vsy, vsz) as vs) // Velocity of the sphere
    : (float<s> list) =

    let R = cyl_r + sph_r
    let inline sq (x : float<'M>) : float<'M^2> = x * x

    // Big complex formula I built using Maxima. Here are the steps:
    (* let D(t,k) be the vectors between the center of the sphere at time t
       and the point on the axis of the cylinder at k (k = 0 is the base, k = 1 is the top).
       Solve (D(t,k) * axis of the cylinder) for k. Let the solutions be K(t).
       Solve D(t, K(t))^2 - R^2 for t.
       The resulting expressions will be too big, but can be reduced using "optimize". *)
    let tmp1 = sq Ay
    let tmp2 = sq Az
    let tmp3 = tmp2 + tmp1
    let tmp4 = sq vcx
    let tmp5 = sq Ax
    let tmp6 = tmp2 + tmp5
    let tmp7 = sq vcy
    let tmp8 = tmp1 + tmp5
    let tmp9 = sq vcz
    let tmp10 = -2.0 * tmp1
    let tmp11 = -2.0 * tmp2
    let tmp12 = sq vsx
    let tmp13 = -2.0 * tmp5
    let tmp14 = sq vsy
    let tmp15 = sq vsz
    let tmp16 = tmp8 * tmp15 + (-2.0 * Ay * Az * vsy - 2.0 * Ax * Az * vsx + (tmp10 + tmp13) * vcz + 2.0 * Ay * Az * vcy + 2.0 * Ax * Az * vcx) * vsz + tmp6 * tmp14 + (-2.0 * Ax * Ay * vsx + 2.0 * Ay * Az * vcz + (tmp11 + tmp13) * vcy + 2.0 * Ax * Ay * vcx) * vsy + tmp3 * tmp12 + (2.0 * Ax * Az * vcz + 2.0 * Ax * Ay * vcy + (tmp11 + tmp10) * vcx) * vsx + tmp8 * tmp9 + (-2.0 * Ay * Az * vcy - 2.0 * Ax * Az * vcx) * vcz + tmp6 * tmp7 - 2.0 * Ax * Ay * vcx * vcy + tmp3 * tmp4
    let tmp17 = 1.0/tmp16
    let tmp18 = -tmp1
    let tmp19 = -tmp2
    let tmp20 = tmp19 + tmp18
    let tmp22 = -tmp5
    let tmp23 = tmp19 + tmp22
    let tmp24 = Ay * Az * sph_z + tmp23 * sph_y + Ax * Ay * sph_x - Ay * Az * cyl_z + tmp6 * cyl_y - Ax * Ay * cyl_x
    let tmp26 = -tmp8 * sph_z + Ay * Az * sph_y + Ax * Az * sph_x + tmp8 * cyl_z - Ay * Az * cyl_y - Ax * Az * cyl_x
    let tmp27 = -Ax * Az * sph_z - Ax * Ay * sph_y + tmp3 * sph_x + Ax * Az * cyl_z + Ax * Ay * cyl_y + tmp20 * cyl_x
    let tmp30 = sq cyl_y
    let tmp31 = sq cyl_z
    let tmp32 = 2.0 * tmp2 * cyl_y - 2.0 * Ay * Az * cyl_z
    let tmp33 = sq sph_y
    let tmp34 = -2.0 * Ay * Az * cyl_y
    let tmp35 = 2.0 * tmp1 * cyl_z
    let tmp36 = sq sph_z
    let tmp37 = -tmp1 * tmp36 + (2.0 * Ay * Az * sph_y + tmp35 + tmp34) * sph_z - tmp2 * tmp33 + tmp32 * sph_y - tmp1 * tmp31 + 2.0 * Ay * Az * cyl_y * cyl_z - tmp2 * tmp30
    let tmp38 = -2.0 * Ay * Az * cyl_x
    let tmp39 = -2.0 * Ax * Az * cyl_y
    let tmp40 = 2.0 * Ay * Az * cyl_x
    let tmp41 = 2.0 * Ax * Az * cyl_y
    let tmp42 = -2.0 * Ay * Az * sph_x
    let tmp43 = 2.0 * Ax * Ay * tmp36 + (-2.0 * Ax * Az * sph_y + tmp42 - 4.0 * Ax * Ay * cyl_z + tmp41 + tmp40) * sph_z + (2.0 * tmp2 * sph_x + 2.0 * Ax * Az * cyl_z - 2.0 * tmp2 * cyl_x) * sph_y + (2.0 * Ay * Az * cyl_z - 2.0 * tmp2 * cyl_y) * sph_x + 2.0 * Ax * Ay * tmp31 + (tmp39 + tmp38) * cyl_z + 2.0 * tmp2 * cyl_x * cyl_y
    let tmp44 = sq cyl_x
    let tmp45 = 2.0 * tmp2 * cyl_x
    let tmp46 = -2.0 * Ax * Az * cyl_z
    let tmp47 = sq sph_x
    let tmp48 = 2.0 * Ax * Az * sph_x + 2.0 * tmp5 * cyl_z - 2.0 * Ax * Az * cyl_x
    let tmp49 = -tmp5 * tmp36 + tmp48 * sph_z - tmp2 * tmp47 + (tmp46 + tmp45) * sph_x - tmp5 * tmp31 + 2.0 * Ax * Az * cyl_x * cyl_z - tmp2 * tmp44
    let tmp50 = 2.0 * tmp1 * cyl_x
    let tmp51 = -2.0 * Ax * Ay * cyl_y
    let tmp52 = tmp51 + tmp50
    let tmp53 = 2.0 * Ax * Ay * cyl_z
    let tmp54 = -2.0 * tmp1 * cyl_x
    let tmp55 = 2.0 * Ax * Ay * cyl_y
    let tmp56 = (-2.0 * Ax * Ay * sph_y + 2.0 * tmp1 * sph_x + tmp55 + tmp54) * sph_z + 2.0 * Ax * Az * tmp33 + (tmp42 + tmp53 - 4.0 * Ax * Az * cyl_y + tmp40) * sph_y + (2.0 * Ay * Az * cyl_y - 2.0 * tmp1 * cyl_z) * sph_x + tmp52 * cyl_z + 2.0 * Ax * Az * tmp30 - 2.0 * Ay * Az * cyl_x * cyl_y
    let tmp57 = -2.0 * Ax * Ay * cyl_x
    let tmp58 = 2.0 * tmp5 * cyl_y
    let tmp59 = 2.0 * Ax * Ay * cyl_x
    let tmp60 = -2.0 * tmp5 * cyl_y
    let tmp61 = (2.0 * tmp5 * sph_y - 2.0 * Ax * Ay * sph_x + tmp60 + tmp59) * sph_z + (-2.0 * Ax * Az * sph_x - 2.0 * tmp5 * cyl_z + 2.0 * Ax * Az * cyl_x) * sph_y + 2.0 * Ay * Az * tmp47 + (tmp53 + tmp41 - 4.0 * Ay * Az * cyl_x) * sph_x + (tmp58 + tmp57) * cyl_z - 2.0 * Ax * Az * cyl_x * cyl_y + 2.0 * Ay * Az * tmp44
    let tmp62 = 2.0 * Ax * Ay * sph_x
    let tmp63 = -tmp5 * tmp33 + (tmp62 + tmp58 + tmp57) * sph_y - tmp1 * tmp47 + tmp52 * sph_x - tmp5 * tmp30 + 2.0 * Ax * Ay * cyl_x * cyl_y - tmp1 * tmp44
    let tmp64 = 2.0 * Ay * Az * sph_x
    let tmp65 = -2.0 * Ax * Ay * tmp36 + (2.0 * Ax * Az * sph_y + tmp64 + 4.0 * Ax * Ay * cyl_z + tmp39 + tmp38) * sph_z + (-2.0 * tmp2 * sph_x + tmp46 + tmp45) * sph_y + tmp32 * sph_x - 2.0 * Ax * Ay * tmp31 + (tmp41 + tmp40) * cyl_z - 2.0 * tmp2 * cyl_x * cyl_y
    let tmp66 = -2.0 * Ax * Ay * cyl_z
    let tmp67 = (2.0 * Ax * Ay * sph_y - 2.0 * tmp1 * sph_x + tmp51 + tmp50) * sph_z - 2.0 * Ax * Az * tmp33 + (tmp64 + tmp66 + 4.0 * Ax * Az * cyl_y + tmp38) * sph_y + (tmp35 + tmp34) * sph_x + (tmp55 + tmp54) * cyl_z - 2.0 * Ax * Az * tmp30 + 2.0 * Ay * Az * cyl_x * cyl_y
    let tmp68 = (-2.0 * tmp5 * sph_y + tmp62 + tmp58 + tmp57) * sph_z + tmp48 * sph_y - 2.0 * Ay * Az * tmp47 + (tmp66 + tmp39 + 4.0 * Ay * Az * cyl_x) * sph_x + (tmp60 + tmp59) * cyl_z + 2.0 * Ax * Az * cyl_x * cyl_y - 2.0 * Ay * Az * tmp44
    let delta = tmp16 * sq R + tmp63 * tmp15 + (tmp61 * vsy + tmp56 * vsx + (2.0 * tmp5 * tmp33 + (-4.0 * Ax * Ay * sph_x - 4.0 * tmp5 * cyl_y + 4.0 * Ax * Ay * cyl_x) * sph_y + 2.0 * tmp1 * tmp47 + (4.0 * Ax * Ay * cyl_y - 4.0 * tmp1 * cyl_x) * sph_x + 2.0 * tmp5 * tmp30 - 4.0 * Ax * Ay * cyl_x * cyl_y + 2.0 * tmp1 * tmp44) * vcz + tmp68 * vcy + tmp67 * vcx) * vsz + tmp49 * tmp14 + (tmp43 * vsx + tmp68 * vcz + (2.0 * tmp5 * tmp36 + (-4.0 * Ax * Az * sph_x - 4.0 * tmp5 * cyl_z + 4.0 * Ax * Az * cyl_x) * sph_z + 2.0 * tmp2 * tmp47 + (4.0 * Ax * Az * cyl_z - 4.0 * tmp2 * cyl_x) * sph_x + 2.0 * tmp5 * tmp31 - 4.0 * Ax * Az * cyl_x * cyl_z + 2.0 * tmp2 * tmp44) * vcy + tmp65 * vcx) * vsy + tmp37 * tmp12 + (tmp67 * vcz + tmp65 * vcy + (2.0 * tmp1 * tmp36 + (-4.0 * Ay * Az * sph_y - 4.0 * tmp1 * cyl_z + 4.0 * Ay * Az * cyl_y) * sph_z + 2.0 * tmp2 * tmp33 + (4.0 * Ay * Az * cyl_z - 4.0 * tmp2 * cyl_y) * sph_y + 2.0 * tmp1 * tmp31 - 4.0 * Ay * Az * cyl_y * cyl_z + 2.0 * tmp2 * tmp30) * vcx) * vsx + tmp63 * tmp9 + (tmp61 * vcy + tmp56 * vcx) * vcz + tmp49 * tmp7 + tmp43 * vcx * vcy + tmp37 * tmp4
    let tmp69 = sqrt(tmp2 + tmp1 + tmp5) * sqrt(delta)
    let tmp70 = tmp26 * vsz + tmp24 * vsy - tmp27 * vsx - tmp26 * vcz - tmp24 * vcy + tmp27 * vcx

    // Unfiltered list of times sorted by time value.
    // Includes collisions with the infinite cylinder, including at negative time values.
    let ts =
        if float delta > 0.0 then
            let t0 = -tmp17 * (tmp69 - tmp70)
            let t1 = tmp17 * (tmp69 + tmp70)
    
            [min t0 t1; max t0 t1]
        elif float delta = 0.0 then
            [tmp17 * tmp70]
        else
            []

    // Filtering of time values: check if intersections are above or below the cylinder
    // Also remove intersections before t = 0.
    
    // Dot product.
    let inline ( ** ) (x, y, z) (X, Y, Z) =
        x * X + y * Y + z * Z

    // Vector subtraction.
    let diffV (x : float<'M>, y : float<'M>, z : float<'M>) (X : float<'M>, Y : float<'M>, Z : float<'M>) =
        (x - X, y - Y, z - Z)

    // Projection of the sphere's center on the axis of the cylinder, range is (0, 1).
    let getK (t : float<s>) =
        let _tmp1 = (A ** diffV vs vc)
        let _tmp2 = (A ** diffV sph_pos cyl_pos)        
        (t * _tmp1 + _tmp2) / (tmp1 + tmp2 + tmp5)

    ts
    |> List.filter (fun t -> t >= 0.0<s> && let k = getK t in 0.0 <= k && k <= 1.0)