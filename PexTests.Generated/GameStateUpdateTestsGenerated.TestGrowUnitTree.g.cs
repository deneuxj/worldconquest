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
using Microsoft.FSharp.Core;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.Pex.Framework.Generated;

namespace WorldConquest.PexTests
{
  public partial class GameStateUpdateTestsGenerated {
[TestMethod]
[PexGeneratedBy(typeof(GameStateUpdateTests))]
public void TestGrowUnitTree423()
{
    global::EmbarkOrders.EmbarkOrder embarkOrder;
    Tuple<FSharpOption<global::GameState.UnitIndex>, global::Units.UnitInfo>[] 
      tuples;
    embarkOrder = global::EmbarkOrdersFactory.CreateEmbarkOrder
                      (0, global::GameState.UnitIndex.NewTransported2(0, 0, 0));
    global::EmbarkOrders.EmbarkOrder[] embarkOrders
       = new global::EmbarkOrders.EmbarkOrder[1];
    global::EmbarkOrders.DisembarkOrder[] disembarkOrders
       = new global::EmbarkOrders.DisembarkOrder[0];
    global::Units.UnitInfo[] unitInfos = new global::Units.UnitInfo[0];
    embarkOrders[0] = embarkOrder;
    tuples = ((GameStateUpdateTests)this).TestGrowUnitTree
                 (embarkOrders, disembarkOrders, unitInfos);
    Assert.IsNotNull((object)tuples);
    Assert.AreEqual<int>(0, tuples.Length);
}
[TestMethod]
[PexGeneratedBy(typeof(GameStateUpdateTests))]
[PexRaisedException(typeof(Exception))]
public void TestGrowUnitTreeThrowsException430()
{
    global::EmbarkOrders.EmbarkOrder embarkOrder;
    global::Units.UnitInfo unitInfo;
    Tuple<FSharpOption<global::GameState.UnitIndex>, global::Units.UnitInfo>[] 
      tuples;
    embarkOrder = global::EmbarkOrdersFactory.CreateEmbarkOrder
                      (0, global::GameState.UnitIndex.NewTransported2(0, 0, 0));
    unitInfo = global::UnitsFactory.CreateUnitInfo
                   ((global::HexTiling.HexCoords)null, (float)0, 0, false, 
                    0, (global::Units.BomberTransport)null, 
                    (global::Units.CarriedAircraft[])null, 
                    (global::Units.TransportedUnit[])null);
    global::EmbarkOrders.EmbarkOrder[] embarkOrders
       = new global::EmbarkOrders.EmbarkOrder[1];
    global::EmbarkOrders.DisembarkOrder[] disembarkOrders
       = new global::EmbarkOrders.DisembarkOrder[0];
    global::Units.UnitInfo[] unitInfos = new global::Units.UnitInfo[1];
    embarkOrders[0] = embarkOrder;
    unitInfos[0] = unitInfo;
    tuples = ((GameStateUpdateTests)this).TestGrowUnitTree
                 (embarkOrders, disembarkOrders, unitInfos);
}
  }
}
