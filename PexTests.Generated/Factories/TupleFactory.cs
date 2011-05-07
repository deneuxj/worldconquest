using System;
using Microsoft.Pex.Framework;
using Microsoft.FSharp.Core;

namespace System
{
    /// <summary>A factory for System.Tuple`3[HexTiling+HexCoords,Resource+Resource,Microsoft.FSharp.Core.FSharpOption`1[GameState+PlayerId]] instances</summary>
  public static partial class TupleFactory {
    /// <summary>A factory for System.Tuple`3[HexTiling+HexCoords,Resource+Resource,Microsoft.FSharp.Core.FSharpOption`1[GameState+PlayerId]] instances</summary>
    [PexFactoryMethod(typeof(Tuple<global::HexTiling.HexCoords, global::Resource.Resource, FSharpOption<global::GameState.PlayerId>>))]
    public static Tuple<global::HexTiling.HexCoords, global::Resource.Resource, FSharpOption<global::GameState.PlayerId>> Create(
        global::HexTiling.HexCoords item1_hexCoords,
        global::Resource.Resource item2_resource,
        FSharpOption<global::GameState.PlayerId> item3_fSharpOption
    ) {
      PexAssume.IsNotNull(item1_hexCoords);
      PexAssume.IsNotNull(item2_resource);
      PexAssume.IsNotNull(item3_fSharpOption);

      Tuple<global::HexTiling.HexCoords,
            global::Resource.Resource, FSharpOption<global::GameState.PlayerId>> tuple
         = new Tuple<global::HexTiling.HexCoords,
                     global::Resource.Resource, FSharpOption<global::GameState.PlayerId>>
               (item1_hexCoords, item2_resource, item3_fSharpOption);
      return tuple;

      // TODO: Edit factory method of Tuple`3<HexCoords,Resource,FSharpOption`1<PlayerId>>
      // This method should be able to configure the object in all possible ways.
      // Add as many parameters as needed,
      // and assign their values to each field by using the API.
    }
  }
}
