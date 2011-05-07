using System;
using Microsoft.Pex.Framework;

/// <summary>A factory for HexTiling+HexCoords instances</summary>
public static partial class HexTilingFactory {
  /// <summary>A factory for HexTiling+HexCoords instances</summary>
  [PexFactoryMethod(typeof(global::HexTiling.HexCoords))]
  public static object CreateHexCoords(int x, int y) {
    return HexTiling.HexCoords.NewHexCoords(x, y);

    // TODO: Edit factory method of HexCoords
    // This method should be able to configure the object in all possible ways.
    // Add as many parameters as needed,
    // and assign their values to each field by using the API.
  }
}
