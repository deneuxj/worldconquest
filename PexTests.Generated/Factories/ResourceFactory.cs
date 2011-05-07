using System;
using Microsoft.Pex.Framework;

/// <summary>A factory for Resource+Resource instances</summary>
public static partial class ResourceFactory {
  /// <summary>A factory for Resource+Resource instances</summary>
  [PexFactoryMethod(typeof(global::Resource.Resource))]
  public static object CreateResource(bool isAirfield, bool isFactory, bool isHarbour, bool isWood, bool isOil) {
    return
      isAirfield ? Resource.Resource.Airfield :
      isFactory ? Resource.Resource.Factory :
      isHarbour ? Resource.Resource.Harbour :
      isWood ? Resource.Resource.Wood :
      isOil ? Resource.Resource.Oil :
      Resource.Resource.Iron;
  }
}
