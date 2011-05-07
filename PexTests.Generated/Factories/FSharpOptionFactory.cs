using System;
using Microsoft.Pex.Framework;
using Microsoft.FSharp.Core;

namespace Microsoft.FSharp.Core
{
    /// <summary>A factory for Microsoft.FSharp.Core.FSharpOption`1[GameState+PlayerId] instances</summary>
  public static partial class FSharpOptionFactory {
    /// <summary>A factory for Microsoft.FSharp.Core.FSharpOption`1[GameState+PlayerId] instances</summary>
    [PexFactoryMethod(typeof(FSharpOption<global::GameState.PlayerId>))]
    public static FSharpOption<global::GameState.PlayerId> Create(global::GameState.PlayerId value_playerId) {
      PexAssume.IsNotNull(value_playerId);

      FSharpOption<global::GameState.PlayerId> fSharpOption
         = new FSharpOption<global::GameState.PlayerId>(value_playerId);
      return fSharpOption;
    }
  }
}
