using System;
using Microsoft.Pex.Framework;
using Microsoft.FSharp.Collections;

/// <summary>A factory for Orders+Order instances</summary>
public static partial class OrdersFactory {
  /// <summary>A factory for Orders+Order instances</summary>
  [PexFactoryMethod(typeof(global::Orders.Order))]
  public static object CreateOrder(bool isBomb, bool isBombard, bool isConquer, bool isDirectAttack, bool isDockAt, bool isLandAt, HexTiling.HexCoords coords, HexTiling.HexCoords[] path) {
    PexAssume.IsNotNull(coords);
    PexAssume.IsNotNull(path);
    PexAssume.AreElementsNotNull(path);

    return
      isBomb ? Orders.Order.NewBomb(coords, ListModule.OfArray(path)) :
      isBombard ? Orders.Order.NewBombard(coords) :
      isConquer ? Orders.Order.NewConquer(ListModule.OfArray(path)) :
      isDirectAttack ? Orders.Order.NewDirectAttack(coords, ListModule.OfArray(path)) :
      isDockAt ? Orders.Order.NewDockAt(ListModule.OfArray(path)) :
      isLandAt ? Orders.Order.NewLandAt(ListModule.OfArray(path)) :
      Orders.Order.NewUnload(ListModule.OfArray(path), coords);
  }
}
