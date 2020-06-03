module PoolTest

open NUnit.Framework

open Pool


[<Test>]
let CropTest1 () = 
    let poly = {pts=[|{X=1.0;Y=1.0}; {X=2.0;Y=3.0}; {X=4.0;Y=0.0}|]}
    Assert.AreEqual(Array.sort poly.pts, Array.sort poly.CropX.pts)


[<Test>]
let ``crop poly removing 1 point`` () = 
    let poly = {pts=[| {X=(-1.0);Y=1.0}; {X=2.0;Y=4.0}; {X=4.0;Y=(-4.0)} |] }
    let croppedPoly = {pts=[|{X=0.0;Y=2.0}; {X=2.0;Y=4.0}; {X=4.0;Y=(-4.0)}; {X=0.0;Y=0.0}|]}
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly.CropX.pts)

    let rotate poly = {pts=Array.append (poly.pts.[1..2]) [|poly.pts.[0]|]}
    let poly2 = rotate poly
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly2.CropX.pts)

    let poly3 = rotate poly2
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly2.CropX.pts)

[<Test>]
let ``crop poly removing 2 points`` () = 
    let poly = {pts=[| {X=1.0;Y=1.0}; {X=(-2.0);Y=4.0}; {X=(-4.0);Y=(-4.0)} |] }
    let croppedPoly = {pts=[|{X=0.0;Y=2.0}; {X=0.0;Y=0.0}; {X=1.0;Y=1.0}|]}
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly.CropX.pts)

    let rotate poly = {pts=Array.append (poly.pts.[1..2]) [|poly.pts.[0]|]}
    let poly2 = rotate poly
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly2.CropX.pts)

    let poly3 = rotate poly2
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly2.CropX.pts)

[<Test>]
let ``crop poly point on x=0`` () = 
    let poly = {pts=[| {X=0.0;Y=1.0}; {X=(-2.0);Y=4.0}; {X=(-4.0);Y=(-4.0)} |] }
    let croppedPoly = {pts=[||]}
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly.CropX.pts)

[<Test>]
let ``crop poly removing all points`` () = 
    let poly = {pts=[| {X=(-0.5);Y=1.0}; {X=(-2.0);Y=4.0}; {X=(-4.0);Y=(-4.0)} |] }
    let croppedPoly = {pts=[||]}
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly.CropX.pts)


[<Test>]
let ``crop poly by unit square leaving full square`` () = 
    let poly = {pts=[| {X=(-1.0);Y=1.0}; {X=2.0;Y=4.0}; {X=4.0;Y=(-4.0)} |] }
    let croppedPoly = {pts=[|{X=0.0;Y=1.0}; {X=1.0;Y=1.0}; {X=1.0;Y=0.0}; {X=0.0;Y=0.0}|]}
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly.CropUnitSquare.pts)

[<Test>]
let ``crop poly by unit square leaving half square`` () = 
    let poly = {pts=[| {X=(-1.0);Y=0.5}; {X=2.0;Y=0.5}; {X=4.0;Y=(-4.0)} |] }
    let croppedPoly = {pts=[|{X=0.0;Y=0.5}; {X=1.0;Y=0.5}; {X=1.0;Y=0.0}; {X=0.0;Y=0.0}|]}
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly.CropUnitSquare.pts)

[<Test>]
let ``crop poly by unit square leaving partial`` () = 
    let poly = {pts=[| {X=0.0;Y=0.0}; {X=0.0;Y=2.0}; {X=1.0;Y=(-6.0)} |] }
    let croppedPoly = {pts=[|{X=0.0;Y=0.0}; {X=0.0;Y=1.0}; {X=0.125;Y=1.0}; {X=0.25;Y=0.0}|]}
    Assert.AreEqual(Array.sort croppedPoly.pts, Array.sort poly.CropUnitSquare.pts)

[<Test>]
let fastCosTest () = 
    Assert.AreEqual(1. ,fastCos(0.))
    Assert.AreEqual(0. ,fastCos((3.14159265/2.)))
    Assert.AreEqual(-1., fastCos(3.14159265))
    Assert.AreEqual(0. ,fastCos(3.14159265*3./2.))
    Assert.AreEqual(1. ,fastCos(3.14159265*2.))
    Assert.AreEqual(0. ,fastCos(3.14159265*5./2.))
    Assert.AreEqual(-1., fastCos(3.14159265*3.))
    Assert.AreEqual(0. ,fastCos(-3.14159265/2.))
    Assert.AreEqual(-1., fastCos(-3.14159265))
    Assert.AreEqual(0. ,fastCos(-3.14159265*3./2.))
    Assert.AreEqual(1. ,fastCos(-3.14159265*2.))
    Assert.AreEqual(0. ,fastCos(-3.14159265*5./2.))
    Assert.AreEqual(-1., fastCos(-3.14159265*3.))
