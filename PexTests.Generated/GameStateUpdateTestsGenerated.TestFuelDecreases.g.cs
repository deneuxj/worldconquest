// <auto-generated>
// This file contains automatically generated unit tests.
// Do NOT modify this file manually.
// 
// When Pex is invoked again,
// it might remove or update any previously generated unit tests.
// 
// If the contents of this file becomes outdated, e.g. if it does not
// compile anymore, you may delete this file and invoke Pex again.
// </auto-generated>
using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.Pex.Framework.Generated;
using Microsoft.ExtendedReflection.DataAccess;

namespace WorldConquest.PexTests
{
  public partial class GameStateUpdateTestsGenerated {
[TestMethod]
[PexGeneratedBy(typeof(GameStateUpdateTests))]
[Ignore]
[PexDescription("the test state was: path bounds exceeded")]
public void TestFuelDecreases967()
{
    int[][] intss = new int[2][];
    int[] ints = new int[2];
    intss[0] = ints;
    int[] ints1 = new int[2];
    intss[1] = ints1;
    ((GameStateUpdateTests)this).TestFuelDecreases(intss, false, 
                                                          global::Units.BomberTransport.NewInfantry((global::Units.Health)null));
}
[TestMethod]
[PexGeneratedBy(typeof(GameStateUpdateTests))]
[Ignore]
[PexDescription("the test state was: path bounds exceeded")]
public void TestFuelDecreases322()
{
    int[][] intss = new int[2][];
    int[] ints = new int[2];
    intss[0] = ints;
    int[] ints1 = new int[2];
    intss[1] = ints1;
    ((GameStateUpdateTests)this).TestFuelDecreases
        (intss, PexSafeHelpers.ByteToBoolean((byte)64), 
                global::Units.BomberTransport.NewInfantry((global::Units.Health)null));
}
[TestMethod]
[PexGeneratedBy(typeof(GameStateUpdateTests))]
[Ignore]
[PexDescription("the test state was: path bounds exceeded")]
public void TestFuelDecreases01()
{
    int[][] intss = new int[2][];
    int[] ints = new int[2];
    intss[0] = ints;
    int[] ints1 = new int[2];
    intss[1] = ints1;
    ((GameStateUpdateTests)this).TestFuelDecreases
        (intss, PexSafeHelpers.ByteToBoolean((byte)16), 
                global::Units.BomberTransport.NewBombs(0));
}
  }
}
