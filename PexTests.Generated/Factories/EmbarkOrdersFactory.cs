using System;
using Microsoft.Pex.Framework;
using Microsoft.FSharp.Collections;

/// <summary>A factory for EmbarkOrders+EmbarkOrder instances</summary>
public static partial class EmbarkOrdersFactory {
  /// <summary>A factory for EmbarkOrders+EmbarkOrder instances</summary>
  [PexFactoryMethod(typeof(global::EmbarkOrders.EmbarkOrder))]
  public static global::EmbarkOrders.EmbarkOrder CreateEmbarkOrder(int transporter_i, global::GameState.UnitIndex unit_unitIndex) {
    PexAssume.IsNotNull(unit_unitIndex);
    global::EmbarkOrders.EmbarkOrder embarkOrder
       = new global::EmbarkOrders.EmbarkOrder(transporter_i, unit_unitIndex);
    return embarkOrder;
  }

  /// <summary>A factory for EmbarkOrders+DisembarkOrder instances</summary>
  [PexFactoryMethod(typeof(global::EmbarkOrders.DisembarkOrder))]
  public static object CreateDisembarkOrder(GameState.UnitIndex idx, HexTiling.HexCoords[] path) {
    PexAssume.IsNotNull(idx);
    PexAssume.IsNotNull(path);
    PexAssume.AreElementsNotNull(path);

    return EmbarkOrders.DisembarkOrder.NewDisembark(idx, ListModule.OfArray(path));
  }
}
